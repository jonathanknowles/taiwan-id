# 0.1.0.0

- Replaced the `Location` type with a new `Region` type, reflecting the fact
  that the first character of an identification number represents an
  administrative area with geographic extent, rather than a specific point in
  space. Accordingly, replaced functions `ID.{get,set}Location` with
  `ID.{get,set}Region`.

- Replaced the `Nationality` type with a new `Issuer` type, reflecting the fact
  that the second character of an identification number encodes the government
  authority that issued the number, rather than the nationality of the holder.
  Accordingly, replaced functions `ID.{get,set}Nationality` with
  `ID.{get,set}Issuer`.

- Added `Region.fromChar` and `Region.toChar` for converting between `Region`
  values and their corresponding character codes.

- Fixed the `Show` and `Read` instances for `Region` to correctly handle
  operator precedence, ensuring that `Region` values are wrapped in parentheses
  when necessary.

- Added `Issuer.toText` for printing `Issuer` values in English and Chinese.

- Fixed incorrect use of "Alien Resident Certificate" (外僑居留證) terminology
  in the package description, replacing it with the correct umbrella term
  "Resident Certificate" (居留證).

- Fixed an internal inconsistency in `listToTuple8`, which previously accepted
  lists of more than 8 elements and silently discarded the excess elements. It
  now correctly requires exactly 8 elements.

# 0.0.0.0

- Initial release.
