//! Explanations of the diagnostic codes (v16, milestone 86): one Markdown
//! page per code in `docs/errors/`, embedded so `lughac explain` and the
//! editor work offline and match this compiler.

/// Every code with its explanation, in code order (`docs/errors/<CODE>.md`).
pub const EXPLANATIONS: [(&str, &str); 122] = [
    ("E0101", include_str!("../docs/errors/E0101.md")),
    ("E0102", include_str!("../docs/errors/E0102.md")),
    ("E0103", include_str!("../docs/errors/E0103.md")),
    ("E0104", include_str!("../docs/errors/E0104.md")),
    ("E0105", include_str!("../docs/errors/E0105.md")),
    ("E0106", include_str!("../docs/errors/E0106.md")),
    ("E0107", include_str!("../docs/errors/E0107.md")),
    ("E0108", include_str!("../docs/errors/E0108.md")),
    ("E0109", include_str!("../docs/errors/E0109.md")),
    ("E0110", include_str!("../docs/errors/E0110.md")),
    ("E0111", include_str!("../docs/errors/E0111.md")),
    ("E0112", include_str!("../docs/errors/E0112.md")),
    ("E0113", include_str!("../docs/errors/E0113.md")),
    ("E0201", include_str!("../docs/errors/E0201.md")),
    ("E0202", include_str!("../docs/errors/E0202.md")),
    ("E0203", include_str!("../docs/errors/E0203.md")),
    ("E0204", include_str!("../docs/errors/E0204.md")),
    ("E0205", include_str!("../docs/errors/E0205.md")),
    ("E0206", include_str!("../docs/errors/E0206.md")),
    ("E0207", include_str!("../docs/errors/E0207.md")),
    ("E0208", include_str!("../docs/errors/E0208.md")),
    ("E0209", include_str!("../docs/errors/E0209.md")),
    ("E0210", include_str!("../docs/errors/E0210.md")),
    ("E0211", include_str!("../docs/errors/E0211.md")),
    ("E0301", include_str!("../docs/errors/E0301.md")),
    ("E0302", include_str!("../docs/errors/E0302.md")),
    ("E0303", include_str!("../docs/errors/E0303.md")),
    ("E0304", include_str!("../docs/errors/E0304.md")),
    ("E0305", include_str!("../docs/errors/E0305.md")),
    ("E0306", include_str!("../docs/errors/E0306.md")),
    ("E0307", include_str!("../docs/errors/E0307.md")),
    ("E0308", include_str!("../docs/errors/E0308.md")),
    ("E0309", include_str!("../docs/errors/E0309.md")),
    ("E0310", include_str!("../docs/errors/E0310.md")),
    ("E0311", include_str!("../docs/errors/E0311.md")),
    ("E0312", include_str!("../docs/errors/E0312.md")),
    ("E0313", include_str!("../docs/errors/E0313.md")),
    ("E0314", include_str!("../docs/errors/E0314.md")),
    ("E0315", include_str!("../docs/errors/E0315.md")),
    ("E0316", include_str!("../docs/errors/E0316.md")),
    ("E0317", include_str!("../docs/errors/E0317.md")),
    ("E0318", include_str!("../docs/errors/E0318.md")),
    ("E0319", include_str!("../docs/errors/E0319.md")),
    ("E0320", include_str!("../docs/errors/E0320.md")),
    ("E0321", include_str!("../docs/errors/E0321.md")),
    ("E0322", include_str!("../docs/errors/E0322.md")),
    ("E0323", include_str!("../docs/errors/E0323.md")),
    ("E0324", include_str!("../docs/errors/E0324.md")),
    ("E0325", include_str!("../docs/errors/E0325.md")),
    ("E0326", include_str!("../docs/errors/E0326.md")),
    ("E0327", include_str!("../docs/errors/E0327.md")),
    ("E0328", include_str!("../docs/errors/E0328.md")),
    ("E0329", include_str!("../docs/errors/E0329.md")),
    ("E0330", include_str!("../docs/errors/E0330.md")),
    ("E0331", include_str!("../docs/errors/E0331.md")),
    ("E0332", include_str!("../docs/errors/E0332.md")),
    ("E0401", include_str!("../docs/errors/E0401.md")),
    ("E0402", include_str!("../docs/errors/E0402.md")),
    ("E0403", include_str!("../docs/errors/E0403.md")),
    ("E0404", include_str!("../docs/errors/E0404.md")),
    ("E0405", include_str!("../docs/errors/E0405.md")),
    ("E0406", include_str!("../docs/errors/E0406.md")),
    ("E0407", include_str!("../docs/errors/E0407.md")),
    ("E0408", include_str!("../docs/errors/E0408.md")),
    ("E0409", include_str!("../docs/errors/E0409.md")),
    ("E0410", include_str!("../docs/errors/E0410.md")),
    ("E0411", include_str!("../docs/errors/E0411.md")),
    ("E0412", include_str!("../docs/errors/E0412.md")),
    ("E0413", include_str!("../docs/errors/E0413.md")),
    ("E0414", include_str!("../docs/errors/E0414.md")),
    ("E0415", include_str!("../docs/errors/E0415.md")),
    ("E0416", include_str!("../docs/errors/E0416.md")),
    ("E0417", include_str!("../docs/errors/E0417.md")),
    ("E0418", include_str!("../docs/errors/E0418.md")),
    ("E0419", include_str!("../docs/errors/E0419.md")),
    ("E0420", include_str!("../docs/errors/E0420.md")),
    ("E0421", include_str!("../docs/errors/E0421.md")),
    ("E0422", include_str!("../docs/errors/E0422.md")),
    ("E0423", include_str!("../docs/errors/E0423.md")),
    ("E0424", include_str!("../docs/errors/E0424.md")),
    ("E0425", include_str!("../docs/errors/E0425.md")),
    ("E0426", include_str!("../docs/errors/E0426.md")),
    ("E0427", include_str!("../docs/errors/E0427.md")),
    ("E0428", include_str!("../docs/errors/E0428.md")),
    ("E0429", include_str!("../docs/errors/E0429.md")),
    ("E0430", include_str!("../docs/errors/E0430.md")),
    ("E0431", include_str!("../docs/errors/E0431.md")),
    ("E0432", include_str!("../docs/errors/E0432.md")),
    ("E0433", include_str!("../docs/errors/E0433.md")),
    ("E0434", include_str!("../docs/errors/E0434.md")),
    ("E0435", include_str!("../docs/errors/E0435.md")),
    ("E0436", include_str!("../docs/errors/E0436.md")),
    ("E0437", include_str!("../docs/errors/E0437.md")),
    ("E0438", include_str!("../docs/errors/E0438.md")),
    ("E0439", include_str!("../docs/errors/E0439.md")),
    ("E0440", include_str!("../docs/errors/E0440.md")),
    ("E0441", include_str!("../docs/errors/E0441.md")),
    ("E0442", include_str!("../docs/errors/E0442.md")),
    ("E0443", include_str!("../docs/errors/E0443.md")),
    ("E0444", include_str!("../docs/errors/E0444.md")),
    ("E0445", include_str!("../docs/errors/E0445.md")),
    ("E0446", include_str!("../docs/errors/E0446.md")),
    ("E0447", include_str!("../docs/errors/E0447.md")),
    ("E0448", include_str!("../docs/errors/E0448.md")),
    ("E0501", include_str!("../docs/errors/E0501.md")),
    ("E0502", include_str!("../docs/errors/E0502.md")),
    ("E0503", include_str!("../docs/errors/E0503.md")),
    ("E0504", include_str!("../docs/errors/E0504.md")),
    ("E0505", include_str!("../docs/errors/E0505.md")),
    ("E0506", include_str!("../docs/errors/E0506.md")),
    ("E0507", include_str!("../docs/errors/E0507.md")),
    ("E0508", include_str!("../docs/errors/E0508.md")),
    ("E0509", include_str!("../docs/errors/E0509.md")),
    ("E0510", include_str!("../docs/errors/E0510.md")),
    ("E0511", include_str!("../docs/errors/E0511.md")),
    ("W0101", include_str!("../docs/errors/W0101.md")),
    ("W0102", include_str!("../docs/errors/W0102.md")),
    ("W0103", include_str!("../docs/errors/W0103.md")),
    ("W0104", include_str!("../docs/errors/W0104.md")),
    ("W0105", include_str!("../docs/errors/W0105.md")),
    ("W0106", include_str!("../docs/errors/W0106.md")),
    ("W0107", include_str!("../docs/errors/W0107.md")),
];

/// The explanation of `code` (`E0501`, `e0501`), if it is a code.
pub fn explain(code: &str) -> Option<&'static str> {
    let code = code.to_ascii_uppercase();
    EXPLANATIONS
        .iter()
        .find(|(c, _)| *c == code)
        .map(|(_, text)| *text)
}

/// The title of an explanation: its first line after `# CODE: `.
pub fn title(text: &str) -> &str {
    let first = text.lines().next().unwrap_or("");
    first.split_once(": ").map_or(first, |(_, title)| title)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn codes_are_found_whatever_their_case() {
        assert!(explain("E0501").is_some());
        assert_eq!(explain("e0501"), explain("E0501"));
        assert!(explain("E9999").is_none());
        assert!(explain("").is_none());
    }

    #[test]
    fn titles_come_from_the_first_line() {
        assert_eq!(title("# E0501: assignment\n\ntext"), "assignment");
    }
}
