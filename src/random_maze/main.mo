import Random "mo:base/Random";
import Array "mo:base/Array";
import List "mo:base/List";
import Stack "mo:base/Stack";
import Blob "mo:base/Blob";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

actor {

    type Maze = [[var Bool]];
    type Visited = [[var Bool]];

    let rand = Blob.fromArray([183,120,32,51,76,77,213,50,167,94,95,38,17,141,42,86,
164,179,149,169,94,248,116,115,132,128,248,75,223,59,117,192]);

    func unvisited(i : Nat, j : Nat, m : Maze, v : Visited) : List.List<(Nat,Nat)> {
      let max = m.size() - 1;
      var cs = List.nil<(Nat,Nat)>();
      if (i > 1 and not v[i - 2][j]) cs := List.push((i - 2, j), cs);
      if (i + 1 < max and not v[i + 2][j]) cs := List.push((i + 2, j), cs);
      if (j > 1 and not v[i][j - 2]) cs := List.push((i, j - 2), cs);
      if (j + 1 < max and not v[i][j + 2]) cs := List.push((i, j + 2), cs);
      cs;
    };

    

    private func dfs(i : Nat, j: Nat, m : Maze, v : Visited, f : Random.Finite) : /* async */ () {
        v[i][j] := true;
        Debug.print(toText(m));
        loop {
          let us = unvisited(i, j, m, v);
          if (List.isNil(us)) return;
          switch (f.range(2)) { 
            case (? k) {
              let ? u = List.get(us, k % List.size(us));
              m[if (i == u.0) i else (Nat.min(i, u.0) + 1)][if (j == u.1) j else (Nat.min(j, u.1)+1)] := false; 
              /* await */ dfs(u.0, u.1, m, v, f);
            };
            case null  {
              return /* await */ dfs(i, j, m, v, Random.Finite(rand /* await Random.blob() */))
            };
          };
        };
    };

    func toText(maze : Maze) : Text {
      var t = "";
      for (row in maze.vals()) {
        for (col in row.vals()) {
          t #= if (col) "█" else " ";
        };
	    t #= "\n";  
      };
      t
    };

    public query func get() : async Text {
      let n = 16;
      let m = Array.tabulate<[var Bool]>(2 * n + 1, func i { Array.init(2 * n + 1, false) });
      for (i in m.keys()) {
          for (j in m[i].keys()) {
              m[i][j] := 
                i == 0 or
                i == 2 * n or 
                j == 0 or
                j == 2 * n or
                (i * (2 * n + 1) + j) % 2 == 1
          }
      };
      let v = Array.tabulate<[var Bool]>(2 * n + 1, func i { Array.init(2 * n + 1, false) });
      dfs(1, 1, m, v, Random.Finite(rand/* await Random.blob()*/));
      toText(m);
    };
};
