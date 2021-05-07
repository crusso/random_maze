import Random "mo:base/Random";
import Array "mo:base/Array";

actor {

    type Maze = [[var Bool]];
    type Visited = [[var Bool]];

    func neighbours(i : Nat, j : Nat, maze: Maze) : [(Nat, Nat)] {
        let m = maze.size() - 1;
        if (i == 0) {
            if (j == 0) 
              [(0, 1), (1, 0)]
            else if (j == m) 
              [(0, m - 1), (1,  m)]
            else 
              [(0, m - 1), (0, m + 1), (1, m)]
        }
        else if (i == m) {
          if (j == 0) 
               [(m - 1, 0), (m, 1)]
            else if (j == m)
               [(m - 1, m), (m, m - 1)]
            else 
               [(m - 1, j), (m, j - 1), (m, j + 1)]
        }
        else [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1) ]
    };

    private func dfs(i : Nat, j: Nat, m : Maze, v : Visited, f : Random.Finite) : async () {
        v[i][j] := true;
        for ((i1, j1) in neighbours(i, j, m).vals()) {
            if (v[i1][j1]) {}
            else switch (f.coin()) { 
                case (? true) {
                    m[i][j] := true; 
                    return await dfs(i1, j1, m, v, f);
                };
                case (? false) {};
                case null  {
                    return await dfs(i, j, m , v, Random.Finite(await Random.blob()))
                }
            };
        };
    };

    func toText(maze : Maze) : Text {
      var t = "";
      for (row in maze.vals()) {
        for (col in row.vals()) {
          t #= if (col) "0" else "1";
        };
	    t #= "\n";  
      };
      t
    };

    public func get() : async Text {
      let n = 4;
      let m = Array.tabulate<[var Bool]>(n, func i { Array.init(n, false) });
      let v = Array.tabulate<[var Bool]>(n, func i { Array.init(n, false) });
      await dfs(n/2, n/2, m, v, Random.Finite(await Random.blob()));
      toText(m);
    };
};
