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

use crate::common::{Priority, Selector};
use crate::util::Date;
use std::fmt::{Debug, Display};
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub struct Args {
  pub action: Action,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Action {
  New {
    title: String,
    priority: Priority,
    labels: Vec<String>,
    blockers: Vec<NewBlockers>,
    blocks: Selector,
  },
  Modify {
    selector: Selector,
    title: Option<String>,
    priority: Option<Priority>,
    new_labels: Vec<String>,
    removed_labels: Vec<String>,
    new_blockers: Vec<NewBlockers>,
    removed_blockers: Vec<RemovedBlockers>,
  },
  List {
    selector: Selector,
  },
  Delete {
    selector: Selector,
  },
}

#[derive(Debug, Eq, PartialEq)]
pub enum NewBlockers {
  Tasks(Selector),
  Date(Date),
  Reason(String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum RemovedBlockers {
  Tasks(Selector),
  Date,
  Reason,
  All,
}

impl FromStr for NewBlockers {
  type Err = <Selector as FromStr>::Err;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s.strip_prefix('~') {
      Some(reason) => Ok(NewBlockers::Reason(reason.into())),
      None => match Date::from_str(s) {
        Ok(date) => Ok(NewBlockers::Date(date)),
        Err(_) => Selector::from_str(s).map(NewBlockers::Tasks),
      },
    }
  }
}

impl FromStr for RemovedBlockers {
  type Err = <Selector as FromStr>::Err;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "~" => Ok(RemovedBlockers::Reason),
      "date" => Ok(RemovedBlockers::Date),
      "all" => Ok(RemovedBlockers::All),
      _ => Selector::from_str(s).map(RemovedBlockers::Tasks),
    }
  }
}

fn app() -> clap::App<'static, 'static> {
  clap::App::new("fina")
    .setting(clap::AppSettings::SubcommandRequiredElseHelp)
    .subcommand(
      clap::SubCommand::with_name("new")
        .aliases(&["add", "create"])
        .about("create a new task")
        .arg(
          clap::Arg::with_name("title")
            .required(true)
            .help("a brief description of the task"),
        )
        .arg(
          clap::Arg::with_name("priority")
            .short("p")
            .long("--priority")
            .takes_value(true)
            .number_of_values(1)
            .validator(validate::<Priority>)
            .help("default, low, high or urgent"),
        )
        .arg(
          clap::Arg::with_name("labels")
            .short("l")
            .long("--label")
            .takes_value(true)
            .number_of_values(1)
            .multiple(true)
            .help("a category in which to put the task"),
        )
        .arg(
          clap::Arg::with_name("blockers")
            .short("b")
            .long("--blocked-by")
            .takes_value(true)
            .number_of_values(1)
            .multiple(true)
            .validator(validate::<NewBlockers>)
            .help("a task selector, date, or arbitrary reason prefixed by '~'"),
        )
        .arg(
          clap::Arg::with_name("blocks")
            .long("--blocks")
            .takes_value(true)
            .number_of_values(1)
            .multiple(true)
            .validator(validate::<Selector>)
            .help("a task selector"),
        ),
    )
    .subcommand(
      clap::SubCommand::with_name("modify")
        .aliases(&["edit", "change"])
        .about("modify one or more existing tasks")
        .arg(
          clap::Arg::with_name("selector")
            .required(true)
            .validator(validate::<Selector>),
        )
        .arg(
          clap::Arg::with_name("title")
            .short("t")
            .long("--title")
            .takes_value(true)
            .help("a brief description of the task"),
        )
        .arg(
          clap::Arg::with_name("priority")
            .short("p")
            .long("--priority")
            .takes_value(true)
            .number_of_values(1)
            .validator(validate::<Priority>)
            .help("default, low, high or urgent"),
        )
        .arg(
          clap::Arg::with_name("new_labels")
            .short("l")
            .long("--label")
            .takes_value(true)
            .number_of_values(1)
            .multiple(true)
            .help("a category in which to put the task"),
        )
        .arg(
          clap::Arg::with_name("removed_labels")
            .short("L")
            .long("--unlabel")
            .takes_value(true)
            .number_of_values(1)
            .multiple(true)
            .help("a category to remove the task from"),
        )
        .arg(
          clap::Arg::with_name("new_blockers")
            .short("b")
            .long("--blocked-by")
            .takes_value(true)
            .number_of_values(1)
            .multiple(true)
            .validator(validate::<NewBlockers>)
            .help("a task selector, date, or arbitrary reason prefixed by '~'"),
        )
        .arg(
          clap::Arg::with_name("removed_blockers")
            .short("B")
            .long("--not-blocked-by")
            .takes_value(true)
            .number_of_values(1)
            .multiple(true)
            .validator(validate::<RemovedBlockers>)
            .help("a task selector, or one of the strings '~', 'date' or 'all'"),
        ),
    )
    .subcommand(
      clap::SubCommand::with_name("list")
        .aliases(&["ls"])
        .about("list tasks")
        .arg(
          clap::Arg::with_name("selector")
            .required(false)
            .validator(validate::<Selector>),
        )
        .arg(
          clap::Arg::with_name("include_blocked")
            .short("a")
            .long("--all")
            .help("show blocked tasks too"),
        ),
    )
    .subcommand(
      clap::SubCommand::with_name("delete")
        .aliases(&["del", "remove", "rm", "complete", "done"])
        .about("delete a task (equivalent to marking it as done)")
        .arg(
          clap::Arg::with_name("selector")
            .required(true)
            .validator(validate::<Selector>),
        ),
    )
}

impl Args {
  pub fn parse<I: Iterator<Item = String>>(args: I) -> clap::Result<Self> {
    let matches = app().get_matches_from_safe(args)?;
    Ok(Args {
      action: match matches.subcommand() {
        ("new", Some(matches)) => Action::New {
          title: matches.value_of_lossy("title").unwrap().into_owned(),
          priority: matches
            .value_of_lossy("priority")
            .map_or(Priority::default(), must_parse),
          labels: matches
            .values_of_lossy("labels")
            .into_iter()
            .flatten()
            .collect(),
          blockers: matches
            .values_of_lossy("blockers")
            .into_iter()
            .flatten()
            .map(must_parse)
            .collect(),
          blocks: Selector::any(
            matches
              .values_of_lossy("blocks")
              .into_iter()
              .flatten()
              .map(must_parse),
          ),
        },
        ("modify", Some(matches)) => Action::Modify {
          selector: must_parse(matches.value_of_lossy("selector").unwrap()),
          title: matches.value_of_lossy("title").map(|s| s.into_owned()),
          priority: matches.value_of_lossy("priority").map(must_parse),
          new_labels: matches
            .values_of_lossy("new_labels")
            .into_iter()
            .flatten()
            .collect(),
          removed_labels: matches
            .values_of_lossy("removed_labels")
            .into_iter()
            .flatten()
            .collect(),
          new_blockers: matches
            .values_of_lossy("new_blockers")
            .into_iter()
            .flatten()
            .map(must_parse)
            .collect(),
          removed_blockers: matches
            .values_of_lossy("removed_blockers")
            .into_iter()
            .flatten()
            .map(must_parse)
            .collect(),
        },
        ("list", Some(matches)) => {
          let selector = matches
            .value_of_lossy("selector")
            .map(must_parse)
            .unwrap_or(Selector::Everything);
          let selector =
            if matches.is_present("include_blocked") || contains_explicit_blocked(&selector) {
              selector
            } else {
              Selector::and(selector, Selector::not(Selector::Blocked))
            };
          Action::List { selector }
        }
        ("delete", Some(matches)) => Action::Delete {
          selector: must_parse(matches.value_of_lossy("selector").unwrap()),
        },
        _ => panic!("unreachable"),
      },
    })
  }
}

fn validate<T>(input: String) -> Result<(), String>
where
  T: FromStr,
  T::Err: Display,
{
  T::from_str(&input).map(drop).map_err(|e| e.to_string())
}

fn must_parse<T, I: AsRef<str>>(input: I) -> T
where
  T: FromStr,
  T::Err: Debug,
{
  T::from_str(input.as_ref()).unwrap()
}

fn contains_explicit_blocked(selector: &Selector) -> bool {
  match selector {
    Selector::Blocked => true,
    Selector::Not(child) => contains_explicit_blocked(child),
    Selector::All(children) => children.iter().any(contains_explicit_blocked),
    Selector::Any(children) => children.iter().any(contains_explicit_blocked),
    _ => false,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn assert_succeeds(input: &[&'static str], expected_output: Args) {
    let input = std::iter::once("progname")
      .chain(input.iter().map(|x| *x))
      .map(String::from);
    match Args::parse(input) {
      Ok(output) => assert_eq!(output, expected_output),
      Err(e) => panic!("unexpected parse error: {}", e),
    }
  }

  fn assert_fails(input: &[&'static str]) {
    let input = std::iter::once("progname")
      .chain(input.iter().map(|x| *x))
      .map(String::from);
    match Args::parse(input) {
      Ok(_) => panic!("expected parse error"),
      Err(_) => {}
    }
  }

  #[test]
  fn test_missing_command() {
    assert_fails(&[])
  }

  #[test]
  fn test_invalid_command() {
    assert_fails(&["mermaid"])
  }

  #[test]
  fn test_new() {
    assert_succeeds(
      &["new", "do the thing"],
      Args {
        action: Action::New {
          title: String::from("do the thing"),
          priority: Priority::Default,
          labels: vec![],
          blockers: vec![],
        },
      },
    )
  }

  #[test]
  fn test_new_missing_title() {
    assert_fails(&["new"])
  }

  #[test]
  fn test_modify() {
    assert_succeeds(
      &["modify", ":123"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_missing_selector() {
    assert_fails(&["modify"])
  }

  #[test]
  fn test_modify_title() {
    assert_succeeds(
      &["modify", ":123", "-t", "new title"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: Some(String::from("new title")),
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_title_missing_argument() {
    assert_fails(&["modify", ":123", "-t"])
  }

  #[test]
  fn test_modify_add_label() {
    assert_succeeds(
      &["modify", ":123", "-l", "mermaid"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![],
          new_labels: vec![String::from("mermaid")],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_remove_label() {
    assert_succeeds(
      &["modify", ":123", "-L", "mermaid"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![],
          new_labels: vec![],
          removed_labels: vec![String::from("mermaid")],
        },
      },
    )
  }

  #[test]
  fn test_modify_label_missing_argument() {
    assert_fails(&["modify", ":123", "-l"])
  }

  #[test]
  fn test_modify_unlabel_missing_argument() {
    assert_fails(&["modify", ":123", "-L"])
  }

  #[test]
  fn test_modify_add_reason_blocker() {
    assert_succeeds(
      &["modify", ":123", "-b", "~mermaid"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![NewBlockers::Reason(String::from("mermaid"))],
          removed_blockers: vec![],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_remove_reason_blocker() {
    assert_succeeds(
      &["modify", ":123", "-B", "~"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![RemovedBlockers::Reason],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_add_date_blocker() {
    assert_succeeds(
      &["modify", ":123", "-b", "2020-11-29"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![NewBlockers::Date(Date::from_ymd(2020, 11, 29))],
          removed_blockers: vec![],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_remove_date_blocker() {
    assert_succeeds(
      &["modify", ":123", "-B", "date"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![RemovedBlockers::Date],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_add_task_blocker() {
    assert_succeeds(
      &["modify", ":123", "-b", ":456"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![NewBlockers::Tasks(Selector::Id(456.into()))],
          removed_blockers: vec![],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_remove_task_blocker() {
    assert_succeeds(
      &["modify", ":123", "-B", ":456"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![RemovedBlockers::Tasks(Selector::Id(456.into()))],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_remove_all_blockers() {
    assert_succeeds(
      &["modify", ":123", "-B", "all"],
      Args {
        action: Action::Modify {
          selector: Selector::Id(123.into()),
          title: None,
          priority: None,
          new_blockers: vec![],
          removed_blockers: vec![RemovedBlockers::All],
          new_labels: vec![],
          removed_labels: vec![],
        },
      },
    )
  }

  #[test]
  fn test_modify_block_missing_argument() {
    assert_fails(&["modify", ":123", "-b"])
  }

  #[test]
  fn test_modify_unblock_missing_argument() {
    assert_fails(&["modify", ":123", "-B"])
  }

  #[test]
  fn test_list() {
    assert_succeeds(
      &["list"],
      Args {
        action: Action::List {
          selector: Selector::not(Selector::Blocked),
        },
      },
    )
  }

  #[test]
  fn test_list_selector() {
    assert_succeeds(
      &["list", ":123"],
      Args {
        action: Action::List {
          selector: Selector::and(Selector::Id(123.into()), Selector::not(Selector::Blocked)),
        },
      },
    )
  }

  #[test]
  fn test_list_all() {
    assert_succeeds(
      &["list", "-a", ":123"],
      Args {
        action: Action::List {
          selector: Selector::Id(123.into()),
        },
      },
    )
  }

  #[test]
  fn test_delete() {
    assert_succeeds(
      &["delete", ":123"],
      Args {
        action: Action::Delete {
          selector: Selector::Id(123.into()),
        },
      },
    )
  }

  #[test]
  fn test_delete_missing_selector() {
    assert_fails(&["delete"])
  }
}
