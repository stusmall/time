//! Rules defined in [ISO 8601].
//!
//! [ISO 8601]: https://www.iso.org/iso-8601-date-and-time-format.html

use core::num::{NonZeroU16, NonZeroU8};

use crate::parsing::combinator::{any_digit, ascii_char, exactly_n_digits, first_match, sign};
use crate::parsing::ParsedItem;
use crate::{Month, Weekday};

/// What kind of format is being parsed. This is used to ensure each part of the format (date, time,
/// offset) is the same kind.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ExtendedKind {
    /// The basic format.
    Basic,
    /// The extended format.
    Extended,
    /// ¯\_(ツ)_/¯
    Unknown,
}

impl ExtendedKind {
    /// Is it possible that the format is extended?
    const fn maybe_extended(self) -> bool {
        matches!(self, Self::Extended | Self::Unknown)
    }

    /// Is the format known for certain to be extended?
    const fn is_extended(self) -> bool {
        matches!(self, Self::Extended)
    }

    /// If the kind is `Unknown`, make it `Basic`. Otherwise, do nothing. Returns `Some` if and only
    /// if the kind is now `Basic`.
    fn coerce_basic(&mut self) -> Option<()> {
        match self {
            Self::Basic => Some(()),
            Self::Extended => None,
            Self::Unknown => {
                *self = Self::Basic;
                Some(())
            }
        }
    }

    /// If the kind is `Unknown`, make it `Extended`. Otherwise, do nothing. Returns `Some` if and
    /// only if the kind is now `Extended`.
    pub(crate) fn coerce_extended(&mut self) -> Option<()> {
        match self {
            Self::Basic => None,
            Self::Extended => Some(()),
            Self::Unknown => {
                *self = Self::Extended;
                Some(())
            }
        }
    }
}

/// Parse a possibly expanded year.
fn year(input: &[u8]) -> Option<ParsedItem<'_, i32>> {
    Some(match sign(input) {
        Some(ParsedItem(input, sign)) => exactly_n_digits::<u32, 6>(input)?.map(|val| {
            let val = val as i32;
            if sign == b'-' { -val } else { val }
        }),
        None => exactly_n_digits::<u32, 4>(input)?.map(|val| val as _),
    })
}

/// Parse a month.
fn month(input: &[u8]) -> Option<ParsedItem<'_, Month>> {
    first_match(
        [
            (&b"01"[..], Month::January),
            (&b"02"[..], Month::February),
            (&b"03"[..], Month::March),
            (&b"04"[..], Month::April),
            (&b"05"[..], Month::May),
            (&b"06"[..], Month::June),
            (&b"07"[..], Month::July),
            (&b"08"[..], Month::August),
            (&b"09"[..], Month::September),
            (&b"10"[..], Month::October),
            (&b"11"[..], Month::November),
            (&b"12"[..], Month::December),
        ],
        true,
    )(input)
}

/// Parse a week number.
fn week(input: &[u8]) -> Option<ParsedItem<'_, NonZeroU8>> {
    exactly_n_digits::<_, 2>(input)
}

/// Parse a day of the month.
fn day(input: &[u8]) -> Option<ParsedItem<'_, NonZeroU8>> {
    exactly_n_digits::<_, 2>(input)
}

/// Parse a day of the week.
fn dayk(input: &[u8]) -> Option<ParsedItem<'_, Weekday>> {
    first_match(
        [
            (&b"1"[..], Weekday::Monday),
            (&b"2"[..], Weekday::Tuesday),
            (&b"3"[..], Weekday::Wednesday),
            (&b"4"[..], Weekday::Thursday),
            (&b"5"[..], Weekday::Friday),
            (&b"6"[..], Weekday::Saturday),
            (&b"7"[..], Weekday::Sunday),
        ],
        true,
    )(input)
}

/// Parse a day of the year.
fn dayo(input: &[u8]) -> Option<ParsedItem<'_, NonZeroU16>> {
    exactly_n_digits::<_, 3>(input)
}

/// Parse the hour.
fn hour(input: &[u8]) -> Option<ParsedItem<'_, u8>> {
    exactly_n_digits::<_, 2>(input)
}

/// Parse the minute.
fn min(input: &[u8]) -> Option<ParsedItem<'_, u8>> {
    exactly_n_digits::<_, 2>(input)
}

/// Parse a floating point number as its integer and optional fractional parts.
///
/// The number must have two digits before the decimal point. If a decimal point is present, at
/// least one digit must follow.
///
/// The return type is a tuple of the integer part and optional fraction part.
fn float(input: &[u8]) -> Option<ParsedItem<'_, (u8, Option<f64>)>> {
    // Two digits before the decimal.
    let ParsedItem(input, integer_part) = match input {
        [
            first_digit @ b'0'..=b'9',
            second_digit @ b'0'..=b'9',
            input @ ..,
        ] => ParsedItem(input, (first_digit - b'0') * 10 + (second_digit - b'0')),
        _ => return None,
    };

    if let Some(ParsedItem(input, ())) = decimal_sign(input) {
        // Mandatory post-decimal digit.
        let ParsedItem(mut input, mut fractional_part) =
            any_digit(input)?.map(|digit| ((digit - b'0') as f64) / 10.);

        let mut divisor = 10.;
        // Any number of subsequent digits.
        while let Some(ParsedItem(new_input, digit)) = any_digit(input) {
            input = new_input;
            divisor *= 10.;
            fractional_part += (digit - b'0') as f64 / divisor;
        }

        Some(ParsedItem(input, (integer_part, Some(fractional_part))))
    } else {
        Some(ParsedItem(input, (integer_part, None)))
    }
}

// Basic: [year][month][day]
// Extended: [year]["-"][month]["-"][day]
/// Parse a calendar date in the basic or extended format.
pub(crate) fn date(
    mut extended_kind: ExtendedKind,
) -> impl FnMut(&[u8]) -> Option<ParsedItem<'_, (ExtendedKind, i32, Month, NonZeroU8)>> {
    move |input| {
        let ParsedItem(mut input, year) = year(input)?;
        if extended_kind.maybe_extended() {
            match ascii_char::<b'-'>(input) {
                Some(ParsedItem(new_input, ())) => {
                    extended_kind.coerce_extended()?;
                    input = new_input;
                }
                None => extended_kind.coerce_basic()?, // no separator before mandatory month
            };
        }
        let ParsedItem(mut input, month) = month(input)?;
        if extended_kind.is_extended() {
            input = ascii_char::<b'-'>(input)?.into_inner();
        }
        let ParsedItem(input, day) = day(input)?;
        Some(ParsedItem(input, (extended_kind, year, month, day)))
    }
}

// Basic: [year][dayo]
// Extended: [year]["-"][dayo]
/// Parse an ordinal date in the basic or extended format.
pub(crate) fn odate(
    mut extended_kind: ExtendedKind,
) -> impl FnMut(&[u8]) -> Option<ParsedItem<'_, (ExtendedKind, i32, NonZeroU16)>> {
    move |input| {
        let ParsedItem(mut input, year) = year(input)?;
        if extended_kind.maybe_extended() {
            match ascii_char::<b'-'>(input) {
                Some(ParsedItem(new_input, ())) => {
                    extended_kind.coerce_extended()?;
                    input = new_input;
                }
                None => extended_kind.coerce_basic()?, // no separator before mandatory day
            };
        }
        let ParsedItem(input, dayo) = dayo(input)?;
        Some(ParsedItem(input, (extended_kind, year, dayo)))
    }
}

// Basic: [year]["W"][week][dayk]
// Extended: [year]["-"]["W"][week]["-"][dayk]
/// Parse a week date in the basic or extended format.
pub(crate) fn wdate(
    mut extended_kind: ExtendedKind,
) -> impl FnMut(&[u8]) -> Option<ParsedItem<'_, (ExtendedKind, i32, NonZeroU8, Weekday)>> {
    move |input| {
        let ParsedItem(mut input, year) = year(input)?;
        if extended_kind.maybe_extended() {
            match ascii_char::<b'-'>(input) {
                Some(ParsedItem(new_input, ())) => {
                    extended_kind.coerce_extended()?;
                    input = new_input;
                }
                None => extended_kind.coerce_basic()?, // no separator before mandatory week
            };
        }
        let input = ascii_char::<b'W'>(input)?.into_inner();
        let ParsedItem(mut input, week) = week(input)?;
        if extended_kind.is_extended() {
            input = ascii_char::<b'-'>(input)?.into_inner();
        }
        let ParsedItem(input, dayk) = dayk(input)?;
        Some(ParsedItem(input, (extended_kind, year, week, dayk)))
    }
}

// Basic: [±][hour][min] or ["Z"]
// Extended: [±][hour][":"][min] or ["Z"]
// Reduced precision: [±][hour] or ["Z"]
/// Parse a UTC offset in the basic or extended format. Reduced precision is supported.
pub(crate) fn shift(
    mut extended_kind: ExtendedKind,
) -> impl FnMut(&[u8]) -> Option<ParsedItem<'_, (ExtendedKind, i8, u8)>> {
    move |input| {
        if let Some(ParsedItem(input, ())) = ascii_char::<b'Z'>(input) {
            return Some(ParsedItem(input, (extended_kind, 0, 0)));
        }

        let ParsedItem(input, sign) = sign(input)?;
        let ParsedItem(mut input, hour) = hour(input)?;

        if extended_kind.maybe_extended() {
            if let Some(ParsedItem(new_input, ())) = ascii_char::<b':'>(input) {
                extended_kind.coerce_extended()?;
                input = new_input;
            };
        }

        let ParsedItem(input, min) = min(input)?;
        // If `:` was present, the format has already been set to extended. As such, this call will
        // do nothing in that case. If there wasn't `:` but minutes were present, we know
        // it's the basic format. Do not use `?` on the call, as returning `None` is valid behavior.
        extended_kind.coerce_basic();

        let hour = if sign == b'-' {
            -(hour as i8)
        } else {
            hour as _
        };

        Some(ParsedItem(input, (extended_kind, hour, min)))
    }
}

// Basic: ["T"][hour][min][sec]
// Extended: ["T"][hour][":"][min][":"][sec]
// Reduced precision: components after [hour] (including their preceding separator) can be omitted.
// ["T"] can be omitted if there is no date present.
/// Parse a time in the basic or extended format. Reduced precision is permitted.
pub(crate) fn time<'a>(
    mut extended_kind: ExtendedKind,
    t_is_required: bool,
) -> impl FnMut(&'a [u8]) -> Option<ParsedItem<'a, (ExtendedKind, u8, u8, u8, u32)>> {
    move |mut input| {
        if t_is_required {
            input = ascii_char::<b'T'>(input)?.into_inner();
        }

        let ParsedItem(mut input, hour) = float(input)?;
        let hour = match hour {
            (hour, None) => hour,
            (hour, Some(fractional_part)) => {
                let minute = fractional_part * 60.;
                let second = fractional_part * 3_600. % 60.;
                let nanosecond = fractional_part * 3_600. * 1_000_000_000. % 1_000_000_000.;
                return Some(ParsedItem(
                    input,
                    (
                        extended_kind,
                        hour,
                        minute as _,
                        second as _,
                        nanosecond as _,
                    ),
                ));
            }
        };

        if let Some(ParsedItem(new_input, ())) = ascii_char::<b':'>(input) {
            extended_kind.coerce_extended()?;
            input = new_input;
        };

        let (mut input, minute) = match float(input) {
            Some(ParsedItem(input, (minute, None))) => {
                // Do not use `?` on the call, as returning `None` is valid behavior.
                extended_kind.coerce_basic();
                (input, minute)
            }
            Some(ParsedItem(input, (minute, Some(fractional_part)))) => {
                // Do not use `?` on the call, as returning `None` is valid behavior.
                extended_kind.coerce_basic();
                let second = fractional_part * 60.;
                let nanosecond = fractional_part * 60. * 1_000_000_000. % 1_000_000_000.;
                return Some(ParsedItem(
                    input,
                    (extended_kind, hour, minute, second as _, nanosecond as _),
                ));
            }
            // colon was present, so minutes are required
            None if extended_kind.is_extended() => return None,
            None => return Some(ParsedItem(input, (extended_kind, hour, 0, 0, 0))),
        };

        if extended_kind.is_extended() {
            input = ascii_char::<b':'>(input)?.into_inner();
        }

        Some(match float(input) {
            Some(ParsedItem(input, (second, None))) => {
                ParsedItem(input, (extended_kind, hour, minute, second, 0))
            }
            Some(ParsedItem(input, (second, Some(fractional_part)))) => {
                let nanosecond = fractional_part * 1_000_000_000.;
                ParsedItem(
                    input,
                    (extended_kind, hour, minute, second, nanosecond as _),
                )
            }
            None => ParsedItem(input, (extended_kind, hour, minute, 0, 0)),
        })
    }
}

/// Parse a "decimal sign", which is either a comma or a period.
fn decimal_sign(input: &[u8]) -> Option<ParsedItem<'_, ()>> {
    ascii_char::<b'.'>(input).or_else(|| ascii_char::<b','>(input))
}
