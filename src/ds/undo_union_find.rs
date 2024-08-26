#[derive(Debug, Clone)]
struct History {
    pub u: i32,
    pub v: i32,
    pub prev_par_u: i32,
    pub prev_par_v: i32
}

impl History {
    fn new(u: i32, v: i32, prev_par_u: i32, prev_par_v: i32) -> History {
        History {
            u,
            v,
            prev_par_u,
            prev_par_v
        }
    }
}

#[derive(Clone, Debug)]
pub struct UndoUnionFind {
    par: Vec<i32>,
    hist: Vec<Option<History>>,
}

impl UndoUnionFind {
    pub fn new(n: usize) -> UndoUnionFind {
        UndoUnionFind {
            par: vec![-1; n],
            hist: Vec::new()
        }
    }

    pub fn merge(&mut self, u: i32, v: i32) {
        let r1 = self.root(u);
        let r2 = self.root(v);
        if r1 == r2 {
            self.hist.push(None);
        } else {
            let size1 = -self.par[r1 as usize];
            let size2 = -self.par[r2 as usize];
            if size1 < size2 {
                self.par[r1 as usize] = r2;
                self.par[r2 as usize] -= size1;
            } else {
                self.par[r1 as usize] -= size2;
                self.par[r2 as usize] = r1;
            }
            self.hist.push(Some(History::new(r1, r2, -size1, -size2)));
        } 
    }

    pub fn is_connected(&self, u: i32, v: i32) -> bool {
        return self.root(u) == self.root(v);
    }

    pub fn undo(&mut self) {
        if let Some(hist) = self.hist.pop().unwrap() {
            self.par[hist.u as usize] = hist.prev_par_u;
            self.par[hist.v as usize] = hist.prev_par_v;
        }
    }

    pub fn root(&self, u: i32) -> i32 {
        let par = self.par[u as usize];
        if par < 0 {
            return u;
        } else {
            return self.root(par);
        }
    }
}
