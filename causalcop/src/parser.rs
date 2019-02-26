use super::history::{Event, History, Session, Transaction};

named!(
    parse_u64<u64>,
    flat_map!(recognize!(nom::digit), parse_to!(u64))
);

named!(
    parse_variable<(u64, u64)>,
    do_parse!(
        char!('(')
            >> id: ws!(parse_u64)
            >> char!(')')
            >> char!(':')
            >> val: ws!(parse_u64)
            >> ((id, val))
    )
);

named!(
    parse_event<Event>,
    delimited!(
        char!('<'),
        ws!(alt!(
            do_parse!(
                char!('W') >> var: parse_variable >> (Event::Write(var.0 as u64, var.1 as u64))
            ) | do_parse!(
                char!('R') >> var: parse_variable >> (Event::Read(var.0 as u64, var.1 as u64))
            )
        )),
        char!('>')
    )
);

named!(
    parse_transaction<Transaction>,
    delimited!(
        char!('['),
        ws!(separated_list!(char!(','), parse_event)),
        tuple!(ws!(opt!(char!(','))), char!(']'))
    )
);

named!(
    parse_session<Session>,
    delimited!(
        char!('['),
        ws!(separated_list!(char!(','), parse_transaction)),
        tuple!(ws!(opt!(char!(','))), char!(']'))
    )
);

named!(
    pub parse_history<History>,
    delimited!(
        char!('['),
        ws!(separated_list!(char!(','), parse_session)),
        tuple!(ws!(opt!(char!(','))), char!(']'))
    )
);
