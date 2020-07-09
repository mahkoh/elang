use num_rational::BigRational;
use num_bigint::BigInt;
use std::ops::{Sub, SubAssign, Add, AddAssign, Mul, Div, Rem, Neg, MulAssign, DivAssign, RemAssign};
use std::convert::TryFrom;
use num_traits::ToPrimitive;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Clone)]
pub struct Number(pub(crate) BigRational);

impl Number {
    pub fn from_f32(f: f32) -> Option<Self> {
        BigRational::from_float(f).map(Number)
    }

    pub fn from_f64(f: f64) -> Option<Self> {
        BigRational::from_float(f).map(Number)
    }

    pub fn trunc(&self) -> Self {
        Self(self.0.trunc())
    }

    pub fn is_integer(&self) -> bool {
        self.0.is_integer()
    }

    pub fn as_big_rational(&self) -> &BigRational {
        &self.0
    }
}

impl From<BigRational> for Number {
    fn from(n: BigRational) -> Self {
        Self(n)
    }
}

impl From<Number> for BigRational {
    fn from(n: Number) -> Self {
        n.0
    }
}

impl AsRef<BigRational> for Number {
    fn as_ref(&self) -> &BigRational {
        &self.0
    }
}

macro_rules! from_int {
    ($ty:ty, $to:ident) => {
        impl From<$ty> for Number {
            fn from(n: $ty) -> Self {
                Self(BigRational::from_integer(n.into()))
            }
        }

        impl Sub<$ty> for Number {
            type Output = Number;

            fn sub(self, rhs: $ty) -> Self::Output {
                self - Number::from(rhs)
            }
        }

        impl Sub<Number> for $ty {
            type Output = Number;

            fn sub(self, rhs: Number) -> Self::Output {
                Number::from(self) - rhs
            }
        }

        impl<'a> Sub<$ty> for &'a Number {
            type Output = Number;

            fn sub(self, rhs: $ty) -> Self::Output {
                self - &Number::from(rhs)
            }
        }

        impl<'a> Sub<Number> for &'a $ty {
            type Output = Number;

            fn sub(self, rhs: Number) -> Self::Output {
                Number::from(*self) - rhs
            }
        }

        impl<'b> Sub<&'b $ty> for Number {
            type Output = Number;

            fn sub(self, rhs: &'b $ty) -> Self::Output {
                self - Number::from(*rhs)
            }
        }

        impl<'b> Sub<&'b Number> for $ty {
            type Output = Number;

            fn sub(self, rhs: &'b Number) -> Self::Output {
                &Number::from(self) - rhs
            }
        }

        impl<'a, 'b> Sub<&'b $ty> for &'a Number {
            type Output = Number;

            fn sub(self, rhs: &'b $ty) -> Self::Output {
                self - &Number::from(*rhs)
            }
        }

        impl<'a, 'b> Sub<&'b Number> for &'a $ty {
            type Output = Number;

            fn sub(self, rhs: &'b Number) -> Self::Output {
                &Number::from(*self) - rhs
            }
        }

        impl SubAssign<$ty> for Number {
            fn sub_assign(&mut self, rhs: $ty) {
                self.0 -= BigInt::from(rhs)
            }
        }

        impl<'b> Add<&'b Number> for $ty {
            type Output = Number;

            fn add(self, rhs: &'b Number) -> Self::Output {
                &Number::from(self) + rhs
            }
        }

        impl<'a, 'b> Add<&'b Number> for &'a $ty {
            type Output = Number;

            fn add(self, rhs: &'b Number) -> Self::Output {
                &Number::from(*self) + rhs
            }
        }

        impl Add<Number> for $ty {
            type Output = Number;

            fn add(self, rhs: Number) -> Self::Output {
                Number::from(self) + rhs
            }
        }

        impl<'a> Add<Number> for &'a $ty {
            type Output = Number;

            fn add(self, rhs: Number) -> Self::Output {
                Number::from(*self) + rhs
            }
        }

        impl<'b> Add<&'b $ty> for Number {
            type Output = Number;

            fn add(self, rhs: &'b $ty) -> Self::Output {
                self + Number::from(*rhs)
            }
        }

        impl<'a, 'b> Add<&'b $ty> for &'a Number {
            type Output = Number;

            fn add(self, rhs: &'b $ty) -> Self::Output {
                self + &Number::from(*rhs)
            }
        }

        impl Add<$ty> for Number {
            type Output = Number;

            fn add(self, rhs: $ty) -> Self::Output {
                self + Number::from(rhs)
            }
        }

        impl<'a> Add<$ty> for &'a Number {
            type Output = Number;

            fn add(self, rhs: $ty) -> Self::Output {
                self + &Number::from(rhs)
            }
        }

        impl AddAssign<$ty> for Number {
            fn add_assign(&mut self, rhs: $ty) {
                self.0 += BigInt::from(rhs)
            }
        }

        impl<'b> Mul<&'b Number> for $ty {
            type Output = Number;

            fn mul(self, rhs: &'b Number) -> Self::Output {
                &Number::from(self) * rhs
            }
        }

        impl<'a, 'b> Mul<&'b Number> for &'a $ty {
            type Output = Number;

            fn mul(self, rhs: &'b Number) -> Self::Output {
                &Number::from(*self) * rhs
            }
        }

        impl Mul<Number> for $ty {
            type Output = Number;

            fn mul(self, rhs: Number) -> Self::Output {
                Number::from(self) * rhs
            }
        }

        impl<'a> Mul<Number> for &'a $ty {
            type Output = Number;

            fn mul(self, rhs: Number) -> Self::Output {
                Number::from(*self) * rhs
            }
        }

        impl<'b> Mul<&'b $ty> for Number {
            type Output = Number;

            fn mul(self, rhs: &'b $ty) -> Self::Output {
                self * Number::from(*rhs)
            }
        }

        impl<'a, 'b> Mul<&'b $ty> for &'a Number {
            type Output = Number;

            fn mul(self, rhs: &'b $ty) -> Self::Output {
                self * &Number::from(*rhs)
            }
        }

        impl Mul<$ty> for Number {
            type Output = Number;

            fn mul(self, rhs: $ty) -> Self::Output {
                self * Number::from(rhs)
            }
        }

        impl<'a> Mul<$ty> for &'a Number {
            type Output = Number;

            fn mul(self, rhs: $ty) -> Self::Output {
                self * &Number::from(rhs)
            }
        }

        impl MulAssign<$ty> for Number {
            fn mul_assign(&mut self, rhs: $ty) {
                self.0 *= BigInt::from(rhs)
            }
        }

        impl Div<Number> for $ty {
            type Output = Number;

            fn div(self, rhs: Number) -> Self::Output {
                Number::from(self) / rhs
            }
        }

        impl<'a> Div<Number> for &'a $ty {
            type Output = Number;

            fn div(self, rhs: Number) -> Self::Output {
                Number::from(*self) / rhs
            }
        }

        impl<'b> Div<&'b Number> for $ty {
            type Output = Number;

            fn div(self, rhs: &'b Number) -> Self::Output {
                &Number::from(self) / rhs
            }
        }

        impl<'a, 'b> Div<&'b Number> for &'a $ty {
            type Output = Number;

            fn div(self, rhs: &'b Number) -> Self::Output {
                &Number::from(*self) / rhs
            }
        }

        impl Div<$ty> for Number {
            type Output = Number;

            fn div(self, rhs: $ty) -> Self::Output {
                self / Number::from(rhs)
            }
        }

        impl<'a> Div<$ty> for &'a Number {
            type Output = Number;

            fn div(self, rhs: $ty) -> Self::Output {
                self / &Number::from(rhs)
            }
        }

        impl<'b> Div<&'b $ty> for Number {
            type Output = Number;

            fn div(self, rhs: &'b $ty) -> Self::Output {
                self / Number::from(*rhs)
            }
        }

        impl<'a, 'b> Div<&'b $ty> for &'a Number {
            type Output = Number;

            fn div(self, rhs: &'b $ty) -> Self::Output {
                self / &Number::from(*rhs)
            }
        }

        impl DivAssign<$ty> for Number {
            fn div_assign(&mut self, rhs: $ty) {
                self.0 /= BigInt::from(rhs)
            }
        }

        impl<'b> Rem<&'b Number> for $ty {
            type Output = Number;

            fn rem(self, rhs: &'b Number) -> Self::Output {
                &Number::from(self) % rhs
            }
        }

        impl<'a, 'b> Rem<&'b Number> for &'a $ty {
            type Output = Number;

            fn rem(self, rhs: &'b Number) -> Self::Output {
                &Number::from(*self) % rhs
            }
        }

        impl Rem<Number> for $ty {
            type Output = Number;

            fn rem(self, rhs: Number) -> Self::Output {
                Number::from(self) % rhs
            }
        }

        impl<'a> Rem<Number> for &'a $ty {
            type Output = Number;

            fn rem(self, rhs: Number) -> Self::Output {
                Number::from(*self) % rhs
            }
        }

        impl<'b> Rem<&'b $ty> for Number {
            type Output = Number;

            fn rem(self, rhs: &'b $ty) -> Self::Output {
                self % Number::from(*rhs)
            }
        }

        impl<'a, 'b> Rem<&'b $ty> for &'a Number {
            type Output = Number;

            fn rem(self, rhs: &'b $ty) -> Self::Output {
                self % &Number::from(*rhs)
            }
        }

        impl Rem<$ty> for Number {
            type Output = Number;

            fn rem(self, rhs: $ty) -> Self::Output {
                self % Number::from(rhs)
            }
        }

        impl<'a> Rem<$ty> for &'a Number {
            type Output = Number;

            fn rem(self, rhs: $ty) -> Self::Output {
                self % &Number::from(rhs)
            }
        }

        impl RemAssign<$ty> for Number {
            fn rem_assign(&mut self, rhs: $ty) {
                self.0 %= BigInt::from(rhs)
            }
        }

        impl TryFrom<Number> for $ty {
            type Error = ();

            fn try_from(value: Number) -> Result<Self, Self::Error> {
                if value.is_integer() {
                    if let Some(i) = value.0.to_integer().$to() {
                        return Ok(i)
                    }
                }
                Err(())
            }
        }

        impl<'a> TryFrom<&'a Number> for $ty {
            type Error = ();

            fn try_from(value: &'a Number) -> Result<Self, Self::Error> {
                if value.is_integer() {
                    if let Some(i) = value.0.to_integer().$to() {
                        return Ok(i)
                    }
                }
                Err(())
            }
        }

        impl PartialEq<Number> for $ty {
            fn eq(&self, other: &Number) -> bool {
                &Number::from(*self) == other
            }
        }

        impl PartialEq<$ty> for Number {
            fn eq(&self, other: &$ty) -> bool {
                self == &Number::from(*other)
            }
        }
    }
}

from_int!(i8, to_i8);
from_int!(i16, to_i16);
from_int!(i32, to_i32);
from_int!(i64, to_i64);
from_int!(i128, to_i128);
from_int!(u8, to_u8);
from_int!(u16, to_u16);
from_int!(u32, to_u32);
from_int!(u64, to_u64);
from_int!(u128, to_u128);
from_int!(usize, to_usize);
from_int!(isize, to_isize);

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl<'b> Sub<&'b Number> for Number {
    type Output = Self;

    fn sub(self, rhs: &'b Number) -> Self::Output {
        Self(self.0 - &rhs.0)
    }
}

impl<'a, 'b> Sub<&'b Number> for &'a Number {
    type Output = Number;

    fn sub(self, rhs: &'b Number) -> Self::Output {
        Number(&self.0 - &rhs.0)
    }
}

impl SubAssign for Number {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0
    }
}

impl<'b> SubAssign<&'b Number> for Number {
    fn sub_assign(&mut self, rhs: &'b Self) {
        self.0 -= &rhs.0
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl<'b> Add<&'b Number> for Number {
    type Output = Self;

    fn add(self, rhs: &'b Number) -> Self::Output {
        Self(self.0 + &rhs.0)
    }
}

impl<'a, 'b> Add<&'b Number> for &'a Number {
    type Output = Number;

    fn add(self, rhs: &'b Number) -> Self::Output {
        Number(&self.0 + &rhs.0)
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl<'b> AddAssign<&'b Number> for Number {
    fn add_assign(&mut self, rhs: &'b Self) {
        self.0 += &rhs.0
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl<'b> Mul<&'b Number> for Number {
    type Output = Self;

    fn mul(self, rhs: &'b Number) -> Self::Output {
        Self(self.0 * &rhs.0)
    }
}

impl<'a, 'b> Mul<&'b Number> for &'a Number {
    type Output = Number;

    fn mul(self, rhs: &'b Number) -> Self::Output {
        Number(&self.0 * &rhs.0)
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 *= rhs.0
    }
}

impl<'b> MulAssign<&'b Number> for Number {
    fn mul_assign(&mut self, rhs: &'b Self) {
        self.0 *= &rhs.0
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Number(self.0 / rhs.0)
    }
}

impl<'b> Div<&'b Number> for Number {
    type Output = Self;

    fn div(self, rhs: &'b Number) -> Self::Output {
        Self(self.0 / &rhs.0)
    }
}

impl<'a, 'b> Div<&'b Number> for &'a Number {
    type Output = Number;

    fn div(self, rhs: &'b Number) -> Self::Output {
        Number(&self.0 / &rhs.0)
    }
}

impl DivAssign for Number {
    fn div_assign(&mut self, rhs: Self) {
        self.0 /= rhs.0
    }
}

impl<'b> DivAssign<&'b Number> for Number {
    fn div_assign(&mut self, rhs: &'b Self) {
        self.0 /= &rhs.0
    }
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Number(self.0 % rhs.0)
    }
}

impl<'b> Rem<&'b Number> for Number {
    type Output = Self;

    fn rem(self, rhs: &'b Number) -> Self::Output {
        Self(self.0 % &rhs.0)
    }
}

impl<'a, 'b> Rem<&'b Number> for &'a Number {
    type Output = Number;

    fn rem(self, rhs: &'b Number) -> Self::Output {
        Number(&self.0 % &rhs.0)
    }
}

impl RemAssign for Number {
    fn rem_assign(&mut self, rhs: Self) {
        self.0 %= rhs.0
    }
}

impl<'b> RemAssign<&'b Number> for Number {
    fn rem_assign(&mut self, rhs: &'b Self) {
        self.0 %= &rhs.0
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Number(-self.0)
    }
}

impl<'a> Neg for &'a Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        Number(-&self.0)
    }
}
