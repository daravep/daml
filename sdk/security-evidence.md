# Security tests, by category

## Authorization:
- badly-authorized create is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L73)
- badly-authorized exercise is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L174)
- badly-authorized exercise/create (create is unauthorized) is rejected: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L267)
- badly-authorized exercise/create (exercise is unauthorized) is rejected: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L235)
- badly-authorized exercise/exercise (no implicit authority from outer exercise) is rejected: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L326)
- badly-authorized fetch is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L108)
- badly-authorized lookup is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L131)
- create with no signatories is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L63)
- create with non-signatory maintainers is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L85)
- exercise with no controllers is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L164)
- well-authorized create is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L56)
- well-authorized exercise is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L157)
- well-authorized exercise/create is accepted: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L213)
- well-authorized exercise/exercise is accepted: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L369)
- well-authorized fetch is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L102)
- well-authorized lookup is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L124)

## Availability:
- Tail call optimization: Tail recursion does not blow the scala JVM stack.: [TailCallTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/TailCallTest.scala#L18)

## Confidentiality:
- ensure correct privacy for create node: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L38)
- ensure correct privacy for exercise node (consuming): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L50)
- ensure correct privacy for exercise node (non-consuming): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L71)
- ensure correct privacy for exercise subtree: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L149)
- ensure correct privacy for fetch node: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L93)
- ensure correct privacy for lookup-by-key node (found): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L106)
- ensure correct privacy for lookup-by-key node (not-found): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L127)
- ensure correct privacy for rollback subtree: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L224)

## Integrity:
- Evaluation order of create with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L570)
- Evaluation order of create with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L604)
- Evaluation order of create with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L655)
- Evaluation order of create with create argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L630)
- Evaluation order of create with duplicate contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L521)
- Evaluation order of create with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L545)
- Evaluation order of create with failed precondition: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L503)
- Evaluation order of create_interface with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L777)
- Evaluation order of create_interface with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L811)
- Evaluation order of create_interface with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L862)
- Evaluation order of create_interface with create argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L837)
- Evaluation order of create_interface with duplicate contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L728)
- Evaluation order of create_interface with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L752)
- Evaluation order of create_interface with failed precondition: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L708)
- Evaluation order of exercise by interface of a cached global contract that does not implement the interface.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1879)
- Evaluation order of exercise by interface of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1861)
- Evaluation order of exercise by interface of cached global contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1921)
- Evaluation order of exercise of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1217)
- Evaluation order of exercise of a non-cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L935)
- Evaluation order of exercise of a non-cached global contract with inconsistent key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L979)
- Evaluation order of exercise of a wrongly typed cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1053)
- Evaluation order of exercise of a wrongly typed non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L920)
- Evaluation order of exercise of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1036)
- Evaluation order of exercise of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1159)
- Evaluation order of exercise of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1260)
- Evaluation order of exercise of an wrongly typed local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1178)
- Evaluation order of exercise of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1093)
- Evaluation order of exercise with argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1274)
- Evaluation order of exercise with output exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1302)
- Evaluation order of exercise_by_key of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1582)
- Evaluation order of exercise_by_key of a non-cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1380)
- Evaluation order of exercise_by_key of a wrongly typed cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1473)
- Evaluation order of exercise_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1454)
- Evaluation order of exercise_by_key of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1562)
- Evaluation order of exercise_by_key of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1625)
- Evaluation order of exercise_by_key of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1492)
- Evaluation order of exercise_by_key with argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1640)
- Evaluation order of exercise_by_key with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1715)
- Evaluation order of exercise_by_key with result exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1669)
- Evaluation order of exercise_interface of a cached local contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2053)
- Evaluation order of exercise_interface of a non-cached global contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1786)
- Evaluation order of exercise_interface of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1989)
- Evaluation order of exercise_interface of an local contract not implementing the interface: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2008)
- Evaluation order of exercise_vy_key with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1699)
- Evaluation order of fetch of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2374)
- Evaluation order of fetch of a non-cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2139)
- Evaluation order of fetch of a non-cached global contract with inconsistent key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2174)
- Evaluation order of fetch of a wrongly typed cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2237)
- Evaluation order of fetch of a wrongly typed disclosed contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2406)
- Evaluation order of fetch of a wrongly typed non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2124)
- Evaluation order of fetch of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2221)
- Evaluation order of fetch of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2322)
- Evaluation order of fetch of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2425)
- Evaluation order of fetch of an wrongly typed local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2339)
- Evaluation order of fetch of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2275)
- Evaluation order of fetch_by_key of a cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2564)
- Evaluation order of fetch_by_key of a local contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2630)
- Evaluation order of fetch_by_key of a non-cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2489)
- Evaluation order of fetch_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2545)
- Evaluation order of fetch_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2612)
- Evaluation order of fetch_by_key of an unknown contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2660)
- Evaluation order of fetch_by_key with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2692)
- Evaluation order of fetch_by_key with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2706)
- Evaluation order of fetch_by_key with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2676)
- Evaluation order of fetch_interface of a cached global contract not implementing the interface.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2843)
- Evaluation order of fetch_interface of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2983)
- Evaluation order of fetch_interface of a non-cached global contract that doesn't implement interface.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2751)
- Evaluation order of fetch_interface of a non-cached global contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2770)
- Evaluation order of fetch_interface of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2826)
- Evaluation order of fetch_interface of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2928)
- Evaluation order of fetch_interface of an local contract not implementing the interface: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2945)
- Evaluation order of fetch_interface of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3013)
- Evaluation order of fetch_interface of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2881)
- Evaluation order of lookup_by_key of a cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3130)
- Evaluation order of lookup_by_key of a local contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3195)
- Evaluation order of lookup_by_key of a non-cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3057)
- Evaluation order of lookup_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3112)
- Evaluation order of lookup_by_key of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3178)
- Evaluation order of lookup_by_key of an unknown contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3225)
- Evaluation order of lookup_by_key with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3257)
- Evaluation order of lookup_by_key with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3271)
- Evaluation order of lookup_by_key with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3241)
- Evaluation order of successful create: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L480)
- Evaluation order of successful create_interface: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L684)
- Evaluation order of successful exercise by interface of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1740)
- Evaluation order of successful exercise of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1012)
- Evaluation order of successful exercise of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1136)
- Evaluation order of successful exercise of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L893)
- Evaluation order of successful exercise_by_key of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1428)
- Evaluation order of successful exercise_by_key of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1538)
- Evaluation order of successful exercise_by_key of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1336)
- Evaluation order of successful exercise_interface of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1836)
- Evaluation order of successful exercise_interface of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1965)
- Evaluation order of successful fetch of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2204)
- Evaluation order of successful fetch of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2307)
- Evaluation order of successful fetch of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2101)
- Evaluation order of successful fetch_by_key of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2528)
- Evaluation order of successful fetch_by_key of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2596)
- Evaluation order of successful fetch_by_key of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2444)
- Evaluation order of successful fetch_interface of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2809)
- Evaluation order of successful fetch_interface of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2913)
- Evaluation order of successful fetch_interface of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2727)
- Evaluation order of successful lookup_by_key of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3095)
- Evaluation order of successful lookup_by_key of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3162)
- Evaluation order of successful lookup_by_key of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3033)
- Exceptions, throw/catch.: [ExceptionTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/ExceptionTest.scala#L28)
- Rollback creates cannot be exercise: [EngineTest.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/EngineTest.scala#L2112)
- This checks that type checking in exercise_interface is done after checking activeness.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2033)
- This checks that type checking is done after checking activeness.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1903)
- This checks that type checking is done after checking activeness.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2965)
- contract key behaviour (non-unique mode): [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L411)
- contract key behaviour (unique mode): [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L421)
- contract keys must have a non-empty set of maintainers: [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L226)
- contract keys should be evaluated after ensure clause: [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L194)
- contract keys should be evaluated only when executing create: [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L153)
- ensure builtin operators have the correct type: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L70)
- ensure expression forms have the correct type: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L130)
- exercise-by-interface command is rejected for a: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L188)
- exercise_interface with a contract instance that does not implement the interface fails.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1768)
- ill-formed create API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L176)
- ill-formed create replay command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L121)
- ill-formed create-and-exercise API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L201)
- ill-formed exception definitions are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1710)
- ill-formed exercise API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L181)
- ill-formed exercise replay command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L126)
- ill-formed exercise-by-key API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L192)
- ill-formed exercise-by-key replay command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L133)
- ill-formed expressions are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L476)
- ill-formed fetch command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L180)
- ill-formed fetch-by-key command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L183)
- ill-formed interfaces are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1442)
- ill-formed kinds are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L42)
- ill-formed lookup command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L188)
- ill-formed records are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1855)
- ill-formed templates are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1087)
- ill-formed type synonyms applications are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1834)
- ill-formed type synonyms definitions are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1901)
- ill-formed types are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L122)
- ill-formed variants are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1878)
- well formed create API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L128)
- well formed create replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L92)
- well formed create-and-exercise API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L156)
- well formed exercise API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L133)
- well formed exercise replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L97)
- well formed exercise-by-interface command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L149)
- well formed exercise-by-key API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L141)
- well formed exercise-by-key command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L105)
- well formed fetch replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L157)
- well formed fetch-by-key replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L162)
- well formed lookup replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L167)


