use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::str::FromStr;

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
  fn from(x: chrono::NaiveDate) -> Self {
    Date(x)
  }
}
impl From<Date> for chrono::NaiveDate {
  fn from(x: Date) -> Self {
    x.0
  }
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
    Self::from_str(<&'de str>::deserialize(d)?).map_err(|err| D::Error::custom(err.to_string()))
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
  fn from(x: chrono::NaiveTime) -> Self {
    Time(x)
  }
}
impl From<Time> for chrono::NaiveTime {
  fn from(x: Time) -> Self {
    x.0
  }
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
    Self::from_str(<&'de str>::deserialize(d)?).map_err(|err| D::Error::custom(err.to_string()))
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FrozenSet<T: Eq + Hash>(HashSet<T>);
impl<T: Eq + Hash> FrozenSet<T> {
  pub fn new() -> Self {
    FrozenSet(HashSet::new())
  }
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
  pub fn iter(&self) -> impl Iterator<Item = &T> {
    self.0.iter()
  }
  pub fn len(&self) -> usize {
    self.0.len()
  }
}
impl<T: Eq + Hash> From<HashSet<T>> for FrozenSet<T> {
  fn from(x: HashSet<T>) -> FrozenSet<T> {
    FrozenSet(x)
  }
}
impl<T: Eq + Hash> From<FrozenSet<T>> for HashSet<T> {
  fn from(x: FrozenSet<T>) -> HashSet<T> {
    x.0
  }
}
impl<T: Eq + Hash> FromIterator<T> for FrozenSet<T> {
  fn from_iter<I>(iter: I) -> FrozenSet<T>
  where
    I: IntoIterator<Item = T>,
  {
    FrozenSet(HashSet::from_iter(iter))
  }
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
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}
/*
macro_rules! frozenset {
  () => { crate::util::FrozenSet::new() };
  ($($x:expr),+ $(,)?) => { crate::util::FrozenSet::from_iter(box [$($x),+]) };
}
*/

pub mod try_iter {
  pub trait TryIterator: Iterator + Sized {
    type Ok;
    type Err;
    fn try_next(&mut self) -> Result<Option<Self::Ok>, Self::Err>;
    fn iter<'a>(self) -> Box<dyn Iterator<Item = Result<Self::Ok, Self::Err>> + 'a>
    where
      Self: 'a,
    {
      struct Proxy<T>(T);
      impl<T> Iterator for Proxy<T>
      where
        T: TryIterator,
      {
        type Item = Result<T::Ok, T::Err>;
        fn next(&mut self) -> Option<Result<T::Ok, T::Err>> {
          self.0.try_next().transpose()
        }
      }
      Box::new(Proxy(self))
    }

    fn try_filter<'a, F>(
      self,
      mut f: F,
    ) -> Box<dyn Iterator<Item = Result<Self::Ok, Self::Err>> + 'a>
    where
      Self: 'a,
      F: FnMut(&Self::Ok) -> Result<bool, Self::Err> + 'a,
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
    fn filter_ok<'a, F>(
      self,
      mut f: F,
    ) -> Box<dyn Iterator<Item = Result<Self::Ok, Self::Err>> + 'a>
    where
      Self: 'a,
      F: FnMut(&Self::Ok) -> bool + 'a,
    {
      self.try_filter(move |r| Ok(f(r)))
    }

    fn try_filter_map<'a, F, NewOk>(
      self,
      mut f: F,
    ) -> Box<dyn Iterator<Item = Result<NewOk, Self::Err>> + 'a>
    where
      Self: 'a,
      F: FnMut(Self::Ok) -> Result<Option<NewOk>, Self::Err> + 'a,
    {
      Box::new(self.iter().filter_map(move |r| match r {
        Ok(o) => f(o).transpose(),
        Err(e) => Some(Err(e)),
      }))
    }
    fn filter_map_ok<'a, F, NewOk>(
      self,
      mut f: F,
    ) -> Box<dyn Iterator<Item = Result<NewOk, Self::Err>> + 'a>
    where
      Self: 'a,
      F: FnMut(Self::Ok) -> Option<NewOk> + 'a,
    {
      self.try_filter_map(move |r| Ok(f(r)))
    }

    fn try_for_each<F>(self, mut f: F) -> Result<(), Self::Err>
    where
      F: FnMut(Self::Ok) -> Result<(), Self::Err>,
    {
      self.iter().fold(Ok(()), move |accum, r| {
        accum.and_then(move |_| r).and_then(|o| f(o))
      })
    }
    fn for_each_ok<F>(self, mut f: F) -> Result<(), Self::Err>
    where
      F: FnMut(Self::Ok),
    {
      self.try_for_each(move |r| Ok(f(r)))
    }

    fn try_map<'a, F, NewOk>(
      self,
      mut f: F,
    ) -> Box<dyn Iterator<Item = Result<NewOk, Self::Err>> + 'a>
    where
      Self: 'a,
      F: FnMut(Self::Ok) -> Result<NewOk, Self::Err> + 'a,
    {
      Box::new(self.iter().map(move |r| r.and_then(|o| f(o))))
    }
    fn map_ok<'a, F, NewOk>(
      self,
      mut f: F,
    ) -> Box<dyn Iterator<Item = Result<NewOk, Self::Err>> + 'a>
    where
      Self: 'a,
      F: FnMut(Self::Ok) -> NewOk + 'a,
    {
      self.try_map(move |r| Ok(f(r)))
    }
    fn map_err<'a, F, NewErr>(
      self,
      mut f: F,
    ) -> Box<dyn Iterator<Item = Result<Self::Ok, NewErr>> + 'a>
    where
      Self: 'a,
      F: FnMut(Self::Err) -> NewErr + 'a,
    {
      Box::new(self.iter().map(move |r| r.map_err(|o| f(o))))
    }
  }

  impl<T, Ok, Err> TryIterator for T
  where
    T: Iterator<Item = Result<Ok, Err>>,
  {
    type Ok = Ok;
    type Err = Err;
    fn try_next(&mut self) -> Result<Option<Ok>, Err> {
      self.next().transpose()
    }
    fn iter<'a>(self) -> Box<dyn Iterator<Item = Result<Ok, Err>> + 'a>
    where
      Self: 'a,
    {
      Box::new(self)
    }
  }
}

pub mod colour {
  #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
  pub enum Colour {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
  }

  impl Colour {
    fn ansi_str(&self) -> &'static str {
      use Colour::*;
      match *self {
        Black => "\x1b[21;30m",
        Red => "\x1b[21;31m",
        Green => "\x1b[21;32m",
        Yellow => "\x1b[21;33m",
        Blue => "\x1b[21;34m",
        Magenta => "\x1b[21;35m",
        Cyan => "\x1b[21;36m",
        White => "\x1b[21;37m",
        BrightBlack => "\x1b[1;30m",
        BrightRed => "\x1b[1;31m",
        BrightGreen => "\x1b[1;32m",
        BrightYellow => "\x1b[1;33m",
        BrightBlue => "\x1b[1;34m",
        BrightMagenta => "\x1b[1;35m",
        BrightCyan => "\x1b[1;36m",
        BrightWhite => "\x1b[1;37m",
      }
    }
  }

  pub const TASK_BLOCKED: Colour = Colour::BrightBlack;
  pub const TASK_DEFAULT: Colour = Colour::BrightWhite;
  pub const TASK_LOW: Colour = Colour::BrightCyan;
  pub const TASK_HIGH: Colour = Colour::Yellow;
  pub const TASK_URGENT: Colour = Colour::Red;
  pub const ID: Colour = Colour::BrightMagenta;
  pub const LABEL: Colour = Colour::BrightGreen;

  pub struct Formatter<'a, 'b> {
    inner: &'a mut std::fmt::Formatter<'b>,
    enable: bool,
  }

  impl<'a, 'b> Formatter<'a, 'b> {
    pub fn set_colour(&mut self, c: Colour) -> std::fmt::Result {
      if self.enable {
        self.inner.write_str(c.ansi_str())
      } else {
        Ok(())
      }
    }
    pub fn reset_colour(&mut self) -> std::fmt::Result {
      if self.enable {
        self.inner.write_str("\x1b[0m")
      } else {
        Ok(())
      }
    }
    pub fn write_str(&mut self, data: &str) -> std::fmt::Result {
      self.inner.write_str(data)
    }
    pub fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::fmt::Result {
      self.inner.write_fmt(fmt)
    }
  }

  pub trait Display {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result;
    fn coloured(&self) -> ColouredDisplay<Self> {
      ColouredDisplay(self)
    }
    fn uncoloured(&self) -> UncolouredDisplay<Self> {
      UncolouredDisplay(self)
    }
  }

  #[derive(Debug)]
  pub struct ColouredDisplay<'a, T: ?Sized>(&'a T);
  impl<'a, T: Display> std::fmt::Display for ColouredDisplay<'a, T> {
    fn fmt(&self, inner: &mut std::fmt::Formatter) -> std::fmt::Result {
      self.0.fmt(&mut Formatter {
        inner,
        enable: true,
      })
    }
  }

  #[derive(Debug)]
  pub struct UncolouredDisplay<'a, T: ?Sized>(&'a T);
  impl<'a, T: Display> std::fmt::Display for UncolouredDisplay<'a, T> {
    fn fmt(&self, inner: &mut std::fmt::Formatter) -> std::fmt::Result {
      self.0.fmt(&mut Formatter {
        inner,
        enable: false,
      })
    }
  }
}
