This code is an experiment in the performance characteristics of Discriminated Unions and Active Patterns. It is for [this](https://matthewcrews.com/blog/2022/03/performance-of-dus-and-active-patterns/) blog post. Any critiques or ideas are welcome 😊.



| Method                                          |      Mean |    Error |   StdDev | BranchMispredictions/Op | CacheMisses/Op |      Gen 0 |     Allocated |
| ----------------------------------------------- | --------: | -------: | -------: | ----------------------: | -------------: | ---------: | ------------: |
| DuEncodingRandomAccess                          |  83.09 ms | 1.122 ms | 0.995 ms |               7,414,534 |        400,225 |          - |         175 B |
| StructDuEncodingRandomAccess                    |  83.56 ms | 0.766 ms | 0.640 ms |               7,415,626 |        409,418 |          - |         175 B |
| IntEncodingWithActivePatternRandomAccess        | 134.88 ms | 2.650 ms | 4.845 ms |               8,126,171 |      1,070,592 | 28500.0000 | 240,000,358 B |
| IntEncodingWithPartialActivePatternRandomAccess |  86.43 ms | 0.841 ms | 0.787 ms |               7,995,620 |        406,096 |          - |         191 B |
| DuEncodingLinearAccess                          |  14.86 ms | 0.090 ms | 0.084 ms |                   5,073 |          7,701 |          - |          22 B |
| StructDuEncodingLinearAccess                    |  17.35 ms | 0.151 ms | 0.142 ms |                 119,799 |         13,508 |          - |          36 B |
| IntEncodingWithActivePatternLinearAccess        |  74.67 ms | 1.018 ms | 0.903 ms |                 167,078 |        677,829 | 28571.4286 | 240,000,191 B |
| IntEncodingWithPartialActivePatternLinearAccess |  22.83 ms | 0.372 ms | 0.348 ms |                   8,151 |         33,225 |          - |          45 B |