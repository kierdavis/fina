use std::collections::HashSet;
use std::iter::FromIterator;
use std::hash::{Hash, Hasher};
use std::str::FromStr;
use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Date(chrono::NaiveDate);
impl Date {
  const FORMAT: &'static str = "%Y-%m-%d";
  pub fn from_ymd(year: i32, month: u32, day: u32) -> Self {
    Date(chrono::NaiveDate::from_ymd(year, month, day))
  }
  pub fn today() -> Self {
    Date(chrono::Local::today().naive_local())
  }
}
impl From<chrono::NaiveDate> for Date {
  fn from(x: chrono::NaiveDate) -> Self { Date(x) }
}
impl From<Date> for chrono::NaiveDate {
  fn from(x: Date) -> Self { x.0 }
}
impl std::fmt::Display for Date {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self.0.format(Self::FORMAT))
  }
}
impl FromStr for Date {
  type Err = chrono::format::ParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    chrono::NaiveDate::parse_from_str(s, Self::FORMAT).map(Date)
  }
}
impl Serialize for Date {
  fn serialize<S: serde::ser::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
    self.to_string().serialize(s)
  }
}
impl<'de> Deserialize<'de> for Date {
  fn deserialize<D: serde::de::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
    use serde::de::Error;
    Self::from_str(<&'de str>::deserialize(d)?)
      .map_err(|err| D::Error::custom(err.to_string()))
  }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Time(chrono::NaiveTime);
impl Time {
  const FORMAT: &'static str = "%H:%m:%s";
  pub fn from_hms(hour: u32, min: u32, sec: u32) -> Self {
    Time(chrono::NaiveTime::from_hms(hour, min, sec))
  }
  pub fn now() -> Self {
    Time(chrono::Local::now().time())
  }
}
impl From<chrono::NaiveTime> for Time {
  fn from(x: chrono::NaiveTime) -> Self { Time(x) }
}
impl From<Time> for chrono::NaiveTime {
  fn from(x: Time) -> Self { x.0 }
}
impl std::fmt::Display for Time {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self.0.format(Self::FORMAT))
  }
}
impl FromStr for Time {
  type Err = chrono::format::ParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    chrono::NaiveTime::parse_from_str(s, Self::FORMAT).map(Time)
  }
}
impl Serialize for Time {
  fn serialize<S: serde::ser::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
    self.to_string().serialize(s)
  }
}
impl<'de> Deserialize<'de> for Time {
  fn deserialize<D: serde::de::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
    use serde::de::Error;
    Self::from_str(<&'de str>::deserialize(d)?)
      .map_err(|err| D::Error::custom(err.to_string()))
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FrozenSet<T: Eq + Hash>(HashSet<T>);
impl<T: Eq + Hash> FrozenSet<T> {
  pub fn new() -> Self { FrozenSet(HashSet::new()) }
  pub fn is_empty(&self) -> bool { self.0.is_empty() }
  pub fn iter(&self) -> impl Iterator<Item=&T> { self.0.iter() }
  pub fn len(&self) -> usize { self.0.len() }
}
impl<T: Eq + Hash> From<HashSet<T>> for FrozenSet<T> {
  fn from(x: HashSet<T>) -> FrozenSet<T> { FrozenSet(x) }
}
impl<T: Eq + Hash> From<FrozenSet<T>> for HashSet<T> {
  fn from(x: FrozenSet<T>) -> HashSet<T> { x.0 }
}
impl<T: Eq + Hash> FromIterator<T> for FrozenSet<T> {
  fn from_iter<I>(iter: I) -> FrozenSet<T>
    where I: IntoIterator<Item=T>
  { FrozenSet(HashSet::from_iter(iter)) }
}
impl<T: Eq + Hash> Hash for FrozenSet<T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    // https://stackoverflow.com/questions/20832279/python-frozenset-hashing-algorithm-implementation
    let mut hash: u64 = 1927868237;
    hash = hash.wrapping_mul(self.len() as u64).wrapping_add(1);
    for element in self.iter() {
      let mut element_hasher = std::collections::hash_map::DefaultHasher::new();
      element.hash(&mut element_hasher);
      let element_hash = element_hasher.finish();
      hash ^= (element_hash ^ (element_hash << 16) ^ 89869747).wrapping_mul(3644798167);
    }
    hash = hash.wrapping_mul(69069).wrapping_add(907133923);
    state.write_u64(hash);
  }
}
impl<T: Eq + Hash> IntoIterator for FrozenSet<T> {
  type Item = T;
  type IntoIter = <HashSet<T> as IntoIterator>::IntoIter;
  fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}
/*
macro_rules! frozenset {
  () => { crate::util::FrozenSet::new() };
  ($($x:expr),+ $(,)?) => { crate::util::FrozenSet::from_iter(box [$($x),+]) };
}
*/

pub mod try_iter {
  use std::iter::FromIterator;
  
  pub trait TryFromIterator<Ok, Err>: Sized {
    fn try_from_iter<I>(iter: I) -> Result<Self, Err>
      where I: TryIterator<Ok=Ok, Err=Err>;
  }

  impl<T, Ok, Err> TryFromIterator<Ok, Err> for T where T: FromIterator<Ok> {
    fn try_from_iter<I>(iter: I) -> Result<Self, Err>
      where I: TryIterator<Ok=Ok, Err=Err>
    {
      let mut err = None;
      let collection = Self::from_iter(iter.iter().filter_map(|r| match r {
        Ok(o) => Some(o),
        Err(e) => { err = Some(e); None },
      }));
      match err {
        Some(e) => Err(e),
        None => Ok(collection),
      }
    }
  }

  pub trait TryIterator: Iterator + Sized {
    type Ok;
    type Err;
    fn try_next(&mut self) -> Result<Option<Self::Ok>, Self::Err>;
    fn iter<'a>(self) -> Box<dyn Iterator<Item=Result<Self::Ok, Self::Err>> + 'a>
      where Self: 'a
    {
      struct Proxy<T>(T);
      impl<T> Iterator for Proxy<T> where T: TryIterator {
        type Item = Result<T::Ok, T::Err>;
        fn next(&mut self) -> Option<Result<T::Ok, T::Err>> {
          self.0.try_next().transpose()
        }
      }
      Box::new(Proxy(self))
    }

    fn try_collect<B>(self) -> Result<B, Self::Err>
      where B: TryFromIterator<Self::Ok, Self::Err>
    {
      B::try_from_iter(self)
    }

    fn try_filter<'a, F>(self, mut f: F) -> Box<dyn Iterator<Item=Result<Self::Ok, Self::Err>> + 'a>
      where Self: 'a, F: FnMut(&Self::Ok) -> Result<bool, Self::Err> + 'a
    {
      Box::new(self.iter().filter_map(move |r| match r {
        Ok(o) => match f(&o) {
          Ok(true) => Some(Ok(o)),
          Ok(false) => None,
          Err(e) => Some(Err(e)),
        },
        Err(e) => Some(Err(e)),
      }))
    }
    fn filter_ok<'a, F>(self, mut f: F) -> Box<dyn Iterator<Item=Result<Self::Ok, Self::Err>> + 'a>
      where Self: 'a, F: FnMut(&Self::Ok) -> bool + 'a
    {
      self.try_filter(move |r| Ok(f(r)))
    }

    fn try_filter_map<'a, F, NewOk>(self, mut f: F) -> Box<dyn Iterator<Item=Result<NewOk, Self::Err>> + 'a>
      where Self: 'a, F: FnMut(Self::Ok) -> Result<Option<NewOk>, Self::Err> + 'a
    {
      Box::new(self.iter().filter_map(move |r| match r {
        Ok(o) => f(o).transpose(),
        Err(e) => Some(Err(e)),
      }))
    }
    fn filter_map_ok<'a, F, NewOk>(self, mut f: F) -> Box<dyn Iterator<Item=Result<NewOk, Self::Err>> + 'a>
      where Self: 'a, F: FnMut(Self::Ok) -> Option<NewOk> + 'a
    {
      self.try_filter_map(move |r| Ok(f(r)))
    }

    fn try_for_each<F>(self, mut f: F) -> Result<(), Self::Err>
      where F: FnMut(Self::Ok) -> Result<(), Self::Err>
    {
      self.iter().fold(Ok(()), move |accum, r| accum.and_then(move |_| r).and_then(|o| f(o)))
    }
    fn for_each_ok<F>(self, mut f: F) -> Result<(), Self::Err>
      where F: FnMut(Self::Ok)
    {
      self.try_for_each(move |r| Ok(f(r)))
    }

    fn try_map<'a, F, NewOk>(self, mut f: F) -> Box<dyn Iterator<Item=Result<NewOk, Self::Err>> + 'a>
      where Self: 'a, F: FnMut(Self::Ok) -> Result<NewOk, Self::Err> + 'a
    {
      Box::new(self.iter().map(move |r| r.and_then(|o| f(o))))
    }
    fn map_ok<'a, F, NewOk>(self, mut f: F) -> Box<dyn Iterator<Item=Result<NewOk, Self::Err>> + 'a>
      where Self: 'a, F: FnMut(Self::Ok) -> NewOk + 'a
    {
      self.try_map(move |r| Ok(f(r)))
    }
    fn map_err<'a, F, NewErr>(self, mut f: F) -> Box<dyn Iterator<Item=Result<Self::Ok, NewErr>> + 'a>
      where Self: 'a, F: FnMut(Self::Err) -> NewErr + 'a
    {
      Box::new(self.iter().map(move |r| r.map_err(|o| f(o))))
    }
  }

  impl<T, Ok, Err> TryIterator for T where T: Iterator<Item=Result<Ok, Err>> {
    type Ok = Ok;
    type Err = Err;
    fn try_next(&mut self) -> Result<Option<Ok>, Err> { self.next().transpose() }
    fn iter<'a>(self) -> Box<dyn Iterator<Item=Result<Ok, Err>> + 'a> where Self: 'a { Box::new(self) }
  }
}

pub mod style {
  use colored::{Colorize, ColoredString};
  
  pub fn task_blocked<C: Colorize>(c: C) -> ColoredString { c.bright_black() }
  pub fn task_default<C: Colorize>(c: C) -> ColoredString { c.bright_white() }
  pub fn task_low<C: Colorize>(c: C) -> ColoredString { c.bright_cyan() }
  pub fn task_high<C: Colorize>(c: C) -> ColoredString { c.yellow() }
  pub fn task_urgent<C: Colorize>(c: C) -> ColoredString { c.red() }
  pub fn id<C: Colorize>(c: C) -> ColoredString { c.bright_magenta() }
  pub fn label<C: Colorize>(c: C) -> ColoredString { c.bright_green() }
}
