use rug::{ops::{NegAssign, Pow}, Float};

#[macro_export]
macro_rules! float {
    ($x:expr) => {
        $crate::value::MathemaValue {
            inner: rug::Float::with_val(512, $x)
        }
    };
}

#[derive(Clone, PartialEq, Debug)]
pub struct MathemaValue {
    pub(crate) inner: Float
}

impl std::fmt::Display for MathemaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_float(&self.inner))
    }
}

impl MathemaValue {
    pub fn add(self, rhs: &Self) -> Self {
        MathemaValue { inner: self.inner + rhs.inner.clone() }
    }

    pub fn sub(self, rhs: &Self) -> Self {
        MathemaValue { inner: self.inner - rhs.inner.clone() }
    }

    pub fn mul(self, rhs: &Self) -> Self {
        MathemaValue { inner: self.inner * rhs.inner.clone() }
    }

    pub fn div(self, rhs: &Self) -> Self {
        MathemaValue { inner: self.inner / rhs.inner.clone() }
    }

    pub fn pow(self, exp: &Self) -> Self {
        MathemaValue { inner: self.inner.pow(exp.inner.clone()) }
    }

    pub fn neg(mut self) -> Self {
        self.inner.neg_assign();
        self
    }
}

fn format_float(input: &Float) -> String {
    if input.clone().abs().log10() < 0f64 {
        return input.to_f64().to_string();
    }

    let input_str = input.to_string();
    let mut result = if input_str.contains('.') {
        input_str
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        input_str
    };

    if let Some(dot_index) = result.find('.') {
        let decimal_count = result.len() - dot_index;
        if decimal_count > 10 {
            result = result[..(result.len() - decimal_count + 10)].to_string();
        }
    }
    result
}
