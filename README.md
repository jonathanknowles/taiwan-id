# `taiwan-id`

[![Latest Release](
  https://img.shields.io/hackage/v/taiwan-id?label=Latest%20Release&color=227755
)](https://hackage.haskell.org/package/taiwan-id)
[![Development Branch](
  https://img.shields.io/badge/Development%20Branch-API%20Documentation-225577
)](https://jonathanknowles.github.io/taiwan-id/)

This package provides a Haskell implementation of Taiwan's uniform
identification number format.

This number format is used by both National Identification Cards (國民身分證)
and Alien Resident Certificates (外僑居留證) issued by the Republic of China
(ROC) government to individuals, with numbers assigned under each system
occupying disjoint parts of the same identifier space.

Each identification number consists of a single uppercase letter followed by
nine decimal digits, with the final digit serving as a checksum calculated
according to a standard algorithm.

Example: `A123456789`

This package offers functions for validating, decoding, and encoding these
numbers.

For more details, see:

* https://zh.wikipedia.org/wiki/中華民國國民身分證
* https://en.wikipedia.org/wiki/National_identification_card_(Taiwan)
* https://en.wikipedia.org/wiki/Resident_certificate
