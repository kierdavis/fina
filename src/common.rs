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

use crate::util::try_iter::TryIterator;
use crate::util::{Date, FrozenSet};
use itertools::Itertools;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::str::FromStr;

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct TaskId(u64);
impl From<u64> for TaskId {
  fn from(x: u64) -> Self {
    TaskId(x)
  }
}
impl From<TaskId> for u64 {
  fn from(x: TaskId) -> Self {
    x.0
  }
}
impl std::fmt::Display for TaskId {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, ":{}", self.0)
  }
}
#[derive(Debug)]
pub struct InvalidTaskId;
impl FromStr for TaskId {
  type Err = InvalidTaskId;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let s = s.strip_prefix(':').ok_or(InvalidTaskId)?;
    let n = u64::from_str(s).map_err(|_| InvalidTaskId)?;
    Ok(TaskId(n))
  }
}
impl sakaagari::Key for TaskId {
  fn to_path(&self) -> Cow<Path> {
    Cow::Owned(PathBuf::from(format!("tasks/{}.toml", self.0)))
  }
  fn from_path(path: &Path) -> Option<Self> {
    if path.parent() == Some(Path::new("tasks")) {
      path
        .file_name()
        .and_then(|name| {
          name
            .to_string_lossy()
            .strip_suffix(".toml")
            .and_then(|num| u64::from_str(num).ok())
        })
        .map(TaskId)
    } else {
      None
    }
  }
  type Codec = TaskCodec;
}

#[derive(Debug)]
pub struct TaskCodec;
impl sakaagari::Codec<TaskId> for TaskCodec {
  type Accepted = Task<TaskId>;
  type EncodeError = toml::ser::Error;
  fn encode<'v>(_: &TaskId, value: &'v Task<TaskId>) -> Result<Cow<'v, [u8]>, toml::ser::Error> {
    toml::to_vec(value).map(Cow::Owned)
  }
  type Produced = Task<TaskId>;
  type DecodeError = toml::de::Error;
  fn decode(key: &TaskId, value: &[u8]) -> Result<Task<TaskId>, toml::de::Error> {
    toml::from_slice::<Task<()>>(value).map(|task| task.assign_id(*key))
  }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Priority {
  Low,
  High,
  Urgent,
}
impl Priority {
  fn is_default(&self) -> bool {
    *self == Self::default()
  }
}
impl Default for Priority {
  fn default() -> Self {
    Priority::Low
  }
}
impl Display for Priority {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.write_str(match self {
      Priority::Low => "low",
      Priority::High => "high",
      Priority::Urgent => "urgent",
    })
  }
}
impl FromStr for Priority {
  type Err = InvalidPriority;
  fn from_str(s: &str) -> Result<Priority, InvalidPriority> {
    match s.to_ascii_lowercase().as_str() {
      "l" | "low" => Ok(Self::Low),
      "h" | "high" => Ok(Self::High),
      "u" | "urgent" => Ok(Self::Urgent),
      _ => Err(InvalidPriority),
    }
  }
}
#[derive(Clone, Copy, Debug)]
pub struct InvalidPriority;
impl Display for InvalidPriority {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.write_str("invalid priority; expected one of 'default', 'low', 'high' or 'urgent'")
  }
}
impl std::error::Error for InvalidPriority {}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Task<Id> {
  #[serde(skip)]
  pub id: Id,
  #[serde(default)]
  pub title: String,
  #[serde(default, skip_serializing_if = "HashSet::is_empty")]
  pub labels: HashSet<String>,
  #[serde(default, skip_serializing_if = "Priority::is_default")]
  pub priority: Priority,
  #[serde(default)]
  pub blocked_by: Option<String>,
  #[serde(default)]
  pub blocked_until: Option<Date>,
  #[serde(default, skip_serializing_if = "HashSet::is_empty")]
  pub blocked_on: HashSet<TaskId>,
}
impl Task<TaskId> {
  pub fn sanitised(mut self) -> Self {
    self.blocked_on.remove(&self.id);
    match &self.blocked_until {
      Some(date) if &Date::today() >= date => self.blocked_until = None,
      _ => {}
    }
    self
  }
}
impl<Id> Task<Id> {
  pub fn without_references_to<I>(mut self, ids: I) -> Self
  where
    I: IntoIterator<Item = TaskId>,
  {
    for id in ids {
      self.blocked_on.remove(&id);
    }
    self
  }
  pub fn is_blocked(&self) -> bool {
    match (&self.blocked_by, &self.blocked_until, &self.blocked_on) {
      (Some(_), _, _) => true,
      (_, Some(date), _) if &Date::today() < date => true,
      (_, _, ids) if !ids.is_empty() => true,
      _ => false,
    }
  }
}
impl<Id: Ord> Task<Id> {
  pub fn default_sort_key<'a>(&'a self) -> impl std::cmp::Ord + 'a {
    if self.is_blocked() {
      (false, Priority::default(), &self.id)
    } else {
      (true, self.priority, &self.id)
    }
  }
  pub fn default_cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.default_sort_key().cmp(&other.default_sort_key())
  }
}
impl<Id: Display> Task<Id> {
  pub fn as_line<'a>(&'a self) -> impl crate::util::colour::Display + 'a {
    use crate::util::colour;
    struct Displayer<'a, Id>(&'a Task<Id>);
    impl<'a, Id: Display> colour::Display for Displayer<'a, Id> {
      fn fmt(&self, f: &mut colour::Formatter) -> std::fmt::Result {
        let task_colour = match (self.0.is_blocked(), self.0.priority) {
          (false, Priority::Low) => colour::TASK_LOW,
          (false, Priority::High) => colour::TASK_HIGH,
          (false, Priority::Urgent) => colour::TASK_URGENT,
          (true, _) => colour::TASK_BLOCKED,
        };
        f.set_colour(colour::ID)?;
        write!(f, "{}", self.0.id)?;
        f.set_colour(task_colour)?;
        write!(f, " {}", self.0.title)?;
        if !self.0.labels.is_empty() {
          f.set_colour(colour::LABEL)?;
          for label in self.0.labels.iter().sorted() {
            write!(f, " @{}", label)?;
          }
          f.set_colour(task_colour)?;
        }
        match &self.0.blocked_by {
          Some(x) => write!(f, " (blocked by: {})", x)?,
          None => {}
        }
        match &self.0.blocked_until {
          Some(x) => write!(f, " (blocked until {})", x)?,
          None => {}
        }
        if !self.0.blocked_on.is_empty() {
          write!(f, " (blocked on")?;
          f.set_colour(colour::ID)?;
          for id in self.0.blocked_on.iter().sorted() {
            write!(f, " {}", id)?;
          }
          f.set_colour(task_colour)?;
          write!(f, ")")?;
        }
        f.reset_colour()
      }
    }
    Displayer(self)
  }
}
impl Task<()> {
  fn assign_id(self, id: TaskId) -> Task<TaskId> {
    Task {
      id: id,
      title: self.title,
      labels: self.labels,
      priority: self.priority,
      blocked_by: self.blocked_by,
      blocked_until: self.blocked_until,
      blocked_on: self.blocked_on,
    }
  }
}
impl Default for Task<()> {
  fn default() -> Self {
    Task {
      id: (),
      title: Default::default(),
      labels: Default::default(),
      priority: Default::default(),
      blocked_by: Default::default(),
      blocked_until: Default::default(),
      blocked_on: Default::default(),
    }
  }
}

pub trait TaskAccess: sakaagari::Access {
  fn task_ids<'a>(&'a self) -> Box<dyn Iterator<Item = TaskId> + 'a> {
    self.keys::<TaskId>()
  }
  fn next_task_id(&self) -> TaskId {
    let existing_ids = self.task_ids().map(u64::from).collect::<HashSet<_>>();
    TaskId::from((1..).filter(|x| !existing_ids.contains(x)).next().unwrap())
  }
  fn tasks<'a>(&'a self) -> Box<dyn Iterator<Item = Result<Task<TaskId>, sakaagari::Error>> + 'a> {
    self.values::<TaskId>()
  }
  fn get_task(&self, id: TaskId) -> Result<Task<TaskId>, sakaagari::Error> {
    self.get(&id)
  }
}
impl<T> TaskAccess for T where T: sakaagari::Access {}
pub trait TaskAccessMut: sakaagari::AccessMut + TaskAccess {
  fn add_task(&mut self, task: Task<()>) -> Result<Task<TaskId>, sakaagari::Error> {
    self.save_task(task.assign_id(self.next_task_id()))
  }
  fn save_task(&mut self, task: Task<TaskId>) -> Result<Task<TaskId>, sakaagari::Error> {
    let task = task.sanitised();
    self.put(&task.id, &task)?;
    Ok(task)
  }
  fn delete_task(&mut self, id: TaskId) -> Result<(), sakaagari::Error> {
    self.delete(&id)
  }
  fn try_map_tasks<F, E>(&mut self, mut f: F) -> Result<Vec<Task<TaskId>>, E>
  where
    F: FnMut(Task<TaskId>, &Self) -> Result<Task<TaskId>, E>,
    E: From<sakaagari::Error>,
  {
    self
      .tasks()
      .map_err(E::from)
      .try_filter_map(|old_task| {
        let new_task = f(old_task.clone(), self)?.sanitised();
        Ok(if new_task != old_task {
          Some(new_task)
        } else {
          None
        })
      })
      .collect::<Vec<_>>()
      .into_iter()
      .try_map(move |task| self.save_task(task).map_err(E::from))
      .collect()
  }
  fn map_tasks<F>(&mut self, mut f: F) -> Result<Vec<Task<TaskId>>, sakaagari::Error>
  where
    F: FnMut(Task<TaskId>, &Self) -> Task<TaskId>,
  {
    self.try_map_tasks(move |t, a| Ok(f(t, a)))
  }
}
impl<T> TaskAccessMut for T where T: sakaagari::AccessMut + TaskAccess {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Selector {
  Nothing,
  Everything,
  Blocked,
  Id(TaskId),
  Label(String),
  Word(String),
  Priority(Priority),
  Not(Box<Selector>),
  All(FrozenSet<Selector>),
  Any(FrozenSet<Selector>),
}
impl Selector {
  pub fn not(child: Selector) -> Self {
    match child {
      Selector::Not(x) => *x,
      _ => Selector::Not(Box::new(child)),
    }
  }
  pub fn all<I>(children: I) -> Self
  where
    I: IntoIterator<Item = Selector>,
  {
    let children: FrozenSet<_> = children
      .into_iter()
      .filter(|child| child != &Selector::Everything)
      .collect();
    match children.len() {
      0 => Selector::Everything,
      1 => children.into_iter().next().unwrap(),
      _ => Selector::All(children),
    }
  }
  pub fn and(a: Selector, b: Selector) -> Self {
    Self::all(vec![a, b])
  }
  pub fn any<I>(children: I) -> Self
  where
    I: IntoIterator<Item = Selector>,
  {
    let children: FrozenSet<_> = children
      .into_iter()
      .filter(|child| child != &Selector::Nothing)
      .collect();
    match children.len() {
      0 => Selector::Nothing,
      1 => children.into_iter().next().unwrap(),
      _ => Selector::Any(children),
    }
  }
  pub fn or(a: Selector, b: Selector) -> Self {
    Self::any(vec![a, b])
  }
  pub fn compile(&self) -> Box<dyn Fn(&Task<TaskId>) -> bool> {
    match self {
      Selector::Nothing => Box::new(|_| false),
      Selector::Everything => Box::new(|_| true),
      Selector::Blocked => Box::new(Task::is_blocked),
      Selector::Id(id) => {
        let id = *id;
        Box::new(move |task| task.id == id)
      }
      Selector::Label(label) => {
        let label = label.clone();
        Box::new(move |task| task.labels.contains(&label))
      }
      Selector::Word(word) => {
        let regex = Regex::new(&format!("(?i)\\b{}\\b", word)).unwrap();
        Box::new(move |task| regex.is_match(&task.title))
      }
      Selector::Priority(priority) => {
        let priority = *priority;
        Box::new(move |task| task.priority == priority)
      }
      Selector::Not(child) => {
        let child = child.compile();
        Box::new(move |task| !child(task))
      }
      Selector::All(children) => {
        let children: Vec<_> = children.iter().map(Selector::compile).collect();
        Box::new(move |task| children.iter().all(move |child| child(task)))
      }
      Selector::Any(children) => {
        let children: Vec<_> = children.iter().map(Selector::compile).collect();
        Box::new(move |task| children.iter().any(move |child| child(task)))
      }
    }
  }
  fn fmt_impl(&self, f: &mut std::fmt::Formatter, atom_required: bool) -> std::fmt::Result {
    match self {
      Selector::Nothing => write!(f, "nothing"),
      Selector::Everything => write!(f, "everything"),
      Selector::Blocked => write!(f, "blocked"),
      Selector::Id(id) => id.fmt(f),
      Selector::Label(label) => write!(f, "@{}", label),
      Selector::Word(word) => word.fmt(f),
      Selector::Priority(priority) => priority.fmt(f),
      Selector::Not(child) => {
        f.write_str("not ")?;
        child.fmt_impl(f, true)
      }
      Selector::All(children) => Self::fmt_children(f, children, atom_required, " and "),
      Selector::Any(children) => Self::fmt_children(f, children, atom_required, " or "),
    }
  }
  fn fmt_children(
    f: &mut std::fmt::Formatter,
    children: &FrozenSet<Selector>,
    atom_required: bool,
    separator: &'static str,
  ) -> std::fmt::Result {
    use std::fmt::Write;
    if atom_required {
      f.write_char('(')?;
    }
    for (i, child) in children.iter().enumerate() {
      if i != 0 {
        f.write_str(separator)?;
      }
      child.fmt_impl(f, true)?;
    }
    if atom_required {
      f.write_char(')')?;
    }
    Ok(())
  }
}
impl std::str::FromStr for Selector {
  // Need to flatten to a static String because lalrpop_util::ParseError
  // borrows the input string.
  type Err = String;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    crate::parse::SelectorParser::new()
      .parse(s)
      .map_err(|e| e.to_string())
  }
}
impl std::fmt::Display for Selector {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    self.fmt_impl(f, false)
  }
}
