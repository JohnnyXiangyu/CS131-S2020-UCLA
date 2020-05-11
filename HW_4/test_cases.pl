test cases

plain_tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          [4,1,5,2,3],
          [3,5|[2,1,4]],
          [1,2,3,4,5]],
         counts([2,3,2,1,5], [4|[2,2,2,1]],
                [4,1,2,2,5],
                [2,4,2,2,1])).

plain_tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          [4,1,5,2,3],
          [1,2,3,4,5],
          [3,5,2,1,4]],
         C).

counts([3,3,1,4,2,2],[2,2,3,1,3,3],[2,3,3,1,2,2],[2,2,1,4,3,3])
[T1, [T21, T22, 4|T2t], [T31, T32,T33,T34,5, T36], [T41,T42,T43,T44,T45,2], T5, [T61, 4|T6t]]


plain_tower(4, T,
         counts([4,1,2,2],
                [1,4,2,2],
                [2,3,2,1],
                [3,1,2,2])),
T = [[1,4,3,2],[2,3,1,4],[3,2,4,1],[4,1,2,3]].

tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          Row3,
          [RC41,5|Row4Tail],
          Row5],
         counts(Top, [4|BottomTail],
                [Left1,Left2,Left3,Left4,5],
                Right)).

checkUniqueRows([[2,3,4,5,1],
          [5,4,1,3,2],
          [4,1,5,2,3],
          [1,2,3,4,5],
          [3,5,2,1,4]])

checkUniqueColumns([[2,3,4,5,1],
           [5,4,1,3,2],
           [4,1,5,2,3],
           [1,2,3,4,5],
           [3,5,2,1,4]], 4)

plain_tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          Row3,
          [RC41,5|Row4Tail],
          Row5],
         counts(Top, [4|BottomTail],
                [Left1,Left2,Left3,Left4,5],
                Right)).