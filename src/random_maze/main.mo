import Random "mo:base/Random";
import Array "mo:base/Array";
import List "mo:base/List";
import Stack "mo:base/Stack";

actor {

    type Maze = [[var Bool]];
    type Visited = [[var Bool]];

    func neighbours(i : Int, j : Int) : [(Int, Int)] {
      [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1) ];
    };

    func unvisited(i : Nat, j : Nat, m : Maze, v : Visited) : List.List<(Nat,Nat)> {
      let max = m.size() - 1;
      var cs = List.nil<(Nat,Nat)>();
      if (i > 0 and not v[i - 1][j]) cs := List.push((i - 1 , j), cs);
      if (i < max and not v[i + 1][j]) cs := List.push((i + 1, j), cs);
      if (j > 0 and not v[i][j - 1]) cs := List.push((i, j - 1), cs);
      if (j < max and not v[i][j + 1]) cs := List.push((i, j + 1), cs);
      cs;
    };

    private func dfs(i : Nat, j: Nat, m : Maze, v : Visited, f : Random.Finite) : async () {
        v[i][j] := true;
        loop {
          let us = unvisited(i, j, m, v);
          if (List.isNil(us)) return;
          switch (f.range(4)) { 
            case (? k) {
              let ? u = List.get(us, k % List.size(us));
              m[i][j] := true; 
              return await dfs(u.0, u.1, m, v, f);
            };
            case null  {
              return await dfs(i, j, m, v, Random.Finite(await Random.blob()))
            };
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
