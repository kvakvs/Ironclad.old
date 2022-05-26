# Definitions

## Terms Used in the Book

<dl>
  <dt>Term</dt>
  <dd>A term is any Erlang value. Terms can have different types and subtypes following the type
    hierarchy.</dd>

  <dt>Atom</dt>
  <dd>A special value, stored as a string in a permanent runtime table, while its index is used as
    an immutable integer in the program.
    The integer value is not fixed and varies from run to run, determined by the virtual machine.
    It is only string value which matters for atoms.
    A naked atom is an atom starting with lowercase letter and having no special symbols in it.
    Otherwise an atom must be enclosed in single quotes.</dd>
</dl>

## Term Ordering

Terms in Erlang have order and different term types can be compared mutually.

    number < atom < reference < fun < port < pid < tuple < list < bit string
