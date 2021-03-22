/*
 * Copyright 2021 Kier Ada Davis
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use self::cmdline::{Action, Args, NewBlockers, RemovedBlockers};
use self::common::{Task, TaskAccess, TaskAccessMut, TaskId};
use self::util::colour::Display;
use self::util::try_iter::TryIterator;
use lalrpop_util::lalrpop_mod;
use std::fmt::Write;

mod cmdline;
mod common;
mod util;
lalrpop_mod!(parse);

fn main() {
  std::process::exit(match main_returning_error() {
    Ok(()) => 0,
    Err(err) => {
      eprintln!("{}", err);
      1
    }
  })
}

fn main_returning_error() -> Result<(), Box<dyn std::error::Error>> {
  let args = Args::parse(std::env::args())?;
  let home_dir = dirs::home_dir().ok_or(String::from("unable to determine home directory"))?;
  let store_path = home_dir.join(".tasks");
  let store = sakaagari::open(store_path)?;
  let state = store.current()?;
  let old_state = state.clone();
  let new_state = match &args.action {
    Action::New {
      title,
      priority,
      labels,
      blockers,
    } => {
      let mut task = Task {
        id: (),
        title: title.clone(),
        priority: *priority,
        ..Default::default()
      };
      adjust_labels(&mut task, labels.as_slice(), &[]);
      adjust_blockers(&mut task, blockers.as_slice(), &[], &state)?;
      state.derive(move |state| {
        let task = state.add_task(task)?;
        println!("{}", task.as_line().coloured());
        Ok(sakaagari::CommitMetadata {
          author: AUTHOR,
          committer: AUTHOR,
          message: format!("create task {}", task.as_line().uncoloured()).into(),
        })
      })?
    }
    Action::Modify {
      selector,
      title,
      priority,
      new_labels,
      removed_labels,
      new_blockers,
      removed_blockers,
    } => {
      let filter = selector.compile();
      let mut changed_tasks = Vec::new();
      let state = state.derive(|state| {
        changed_tasks = state.try_map_tasks(|mut task, state| {
          if filter(&task) {
            match title {
              Some(title) => task.title = title.clone(),
              None => {}
            }
            match priority {
              Some(priority) => task.priority = *priority,
              None => {}
            }
            adjust_labels(&mut task, new_labels.as_slice(), removed_labels.as_slice());
            adjust_blockers(
              &mut task,
              new_blockers.as_slice(),
              removed_blockers.as_slice(),
              state,
            )?;
          }
          Ok(task)
        })?;
        Ok(sakaagari::CommitMetadata {
          author: AUTHOR,
          committer: AUTHOR,
          message: match changed_tasks.len() {
            0 => "modify no tasks".into(),
            1 => format!(
              "modify task {}",
              changed_tasks.iter().next().unwrap().as_line().uncoloured()
            )
            .into(),
            _ => {
              changed_tasks.sort_by(Task::default_cmp);
              let mut s = format!(
                "modify {} tasks matching '{}':\n",
                changed_tasks.len(),
                selector
              );
              for task in changed_tasks.iter() {
                let _ = write!(s, "\n{}", task.as_line().uncoloured());
              }
              s.into()
            }
          },
        })
      })?;
      print_tasks(changed_tasks);
      state
    }
    Action::List { selector } => {
      print_tasks(
        state
          .tasks()
          .filter_ok(selector.compile())
          .collect::<Result<Vec<_>, _>>()?,
      );
      state
    }
    Action::Delete { selector } => {
      let mut deleted_tasks = Vec::new();
      let mut side_effect_tasks = Vec::new();
      let state = state.derive(|state| {
        let task_ids_to_delete = state
          .tasks()
          .filter_ok(selector.compile())
          .map_ok(|task| {
            deleted_tasks.push(task.clone());
            task.id
          })
          .collect::<Result<Vec<_>, _>>()?;
        task_ids_to_delete
          .iter()
          .copied()
          .map(Ok)
          .try_for_each(|id| state.delete_task(id))?;
        side_effect_tasks = state
          .map_tasks(|task, _| task.without_references_to(task_ids_to_delete.iter().copied()))?;
        Ok(sakaagari::CommitMetadata {
          author: AUTHOR,
          committer: AUTHOR,
          message: match deleted_tasks.len() {
            0 => "delete no tasks".into(),
            1 => format!(
              "delete task {}",
              deleted_tasks.iter().next().unwrap().as_line().uncoloured()
            )
            .into(),
            _ => {
              deleted_tasks.sort_by(Task::default_cmp);
              let mut s = format!(
                "delete {} tasks matching '{}':\n",
                deleted_tasks.len(),
                selector
              );
              for task in deleted_tasks.iter() {
                let _ = write!(s, "\n{}", task.as_line().uncoloured());
              }
              s.into()
            }
          },
        })
      })?;
      print_tasks(deleted_tasks);
      if !side_effect_tasks.is_empty() {
        println!("The following tasks were modified as a side effect:");
        print_tasks(side_effect_tasks);
      }
      state
    }
  };
  if new_state != old_state {
    store.set_current(&new_state)?;
  }
  Ok(())
}

fn print_tasks(mut tasks: Vec<Task<TaskId>>) {
  tasks.sort_by(Task::default_cmp);
  for task in tasks {
    println!("{}", task.as_line().coloured());
  }
}

fn adjust_labels<'a, Id, I: IntoIterator<Item = &'a String>>(
  task: &mut Task<Id>,
  new: I,
  removed: I,
) {
  for label in removed.into_iter() {
    task.labels.remove(label);
  }
  for label in new.into_iter() {
    task.labels.insert(label.clone());
  }
}

fn adjust_blockers<Id, A: TaskAccess>(
  task: &mut Task<Id>,
  new: &[NewBlockers],
  removed: &[RemovedBlockers],
  access: &A,
) -> Result<(), sakaagari::Error> {
  for b in removed {
    match b {
      RemovedBlockers::Tasks(selector) => access
        .tasks()
        .filter_ok(selector.compile())
        .for_each_ok(|task_to_remove| {
          task.blocked_on.remove(&task_to_remove.id);
        })?,
      RemovedBlockers::Date => task.blocked_until = None,
      RemovedBlockers::Reason => task.blocked_by = None,
      RemovedBlockers::All => {
        task.blocked_on.clear();
        task.blocked_until = None;
        task.blocked_by = None;
      }
    }
  }
  for b in new {
    match b {
      NewBlockers::Tasks(selector) => {
        access
          .tasks()
          .filter_ok(selector.compile())
          .for_each_ok(|task_to_add| {
            task.blocked_on.insert(task_to_add.id);
          })?
      }
      NewBlockers::Date(date) => task.blocked_until = Some(date.clone()),
      NewBlockers::Reason(reason) => task.blocked_by = Some(reason.clone()),
    }
  }
  Ok(())
}

const AUTHOR: sakaagari::Author<'static> = sakaagari::Author {
  name: std::borrow::Cow::Borrowed("Kier Davis"),
  email: std::borrow::Cow::Borrowed("me@kierdavis.com"),
};
