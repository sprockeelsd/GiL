#include "headers/gecode_problem.hpp"

using namespace Gecode;
using namespace std;

int main(int argc, char* argv[]) {
    int size = 3;
    // create a new problem
    Problem* p = new Problem(size);

    // create a new search engine
    DFS<Problem>* e = make_dfs(p);
    delete p;

    int nb_sol = 0;

    while(Problem * pb = e->next()){
        nb_sol++;
        cout << "Solution " << nb_sol << ": " << endl;
        pb->print_solution();
        delete pb;
    }
    cout << "No (more) solutions." << endl;
    return 0;
}

