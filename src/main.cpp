#include "readData.h"
#include <stdio.h>
#include <vector>
#include <algorithm>
#include <math.h>
#include <limits>
#include <chrono>

using namespace std;


struct inserir{
    int no,aresta;
    double custo;
};

bool cmp (inserir a, inserir b){
    return a.custo<b.custo;
}

double ** matrizAdj; // matriz de adjacencia
int dimension; // quantidade total de vertices


void construction(vector<int> &s1,double &custo1){
    vector<int> candidatos;
    custo1=0;
    s1={1};
    for (int i=2;i<=dimension;i++){
        candidatos.push_back(i);
    }
    int tamSubtour=3;
    for (int j=0;j<tamSubtour;j++){

        int e=rand()%candidatos.size();
        custo1+=matrizAdj[s1[j]][candidatos[e]];

        s1.push_back(candidatos[e]);
        candidatos.erase(candidatos.begin()+e);
    }
    custo1+=matrizAdj[s1[tamSubtour]][1];
    s1.push_back(1);
    
    while (tamSubtour<dimension-1){
        vector<inserir> custoInserir((s1.size()-1)*candidatos.size());
        tamSubtour++;
        for (int j=0,l=0;j<s1.size()-1;j++){
            for (int k=0;k<candidatos.size();k++){
                custoInserir[l].no=k;
                custoInserir[l].aresta=j;
                custoInserir[l].custo=matrizAdj[candidatos[k]][s1[j]]+matrizAdj[candidatos[k]][s1[j+1]]-matrizAdj[s1[j]][s1[j+1]];
                l++;
            }
        }
        
        sort(custoInserir.begin(),custoInserir.end(),cmp);
        
        int a=round(((rand() % 100)/100.0)*custoInserir.size());
        if (a==0) a=1;
        
        int e=rand()%a;
        
        s1.insert(s1.begin()+1+custoInserir[e].aresta,candidatos[custoInserir[e].no]);
        candidatos.erase(candidatos.begin()+custoInserir[e].no);
        

        custo1+=custoInserir[e].custo;
        
    }
}

int Swap(vector<int> &s1, double &custo1){

    double d=0,t=0,t0=0;
    int x,y;
    for (int i=1;i<dimension-1;i++){

        t0=-matrizAdj[s1[i-1]][s1[i]];

        for (int j=i+1;j<dimension;j++){

            t=t0+matrizAdj[s1[i-1]][s1[j]]+matrizAdj[s1[i]][s1[j+1]]-matrizAdj[s1[j+1]][s1[j]];
            if (j!=i+1) t+=matrizAdj[s1[i+1]][s1[j]]+matrizAdj[s1[i]][s1[j-1]]-matrizAdj[s1[i+1]][s1[i]]-matrizAdj[s1[j-1]][s1[j]];

            if (t<d) {
                d=t;
                x=i;
                y=j;
            }
        }
    }
    
    if (d<0){
        
        swap(s1[x],s1[y]);
        custo1+=d;
        
        return 0;
    }
    
    return 1;
}

int Opt2(vector<int> &s1, double &custo1){
    
    double d=0,t=0,t0=0;
    int x,y;
    for (int i=0;i<dimension-3;i++){
        
        t0=-matrizAdj[s1[i]][s1[i+1]];

        for (int j=i+3;j<dimension;j++){
            t=t0+matrizAdj[s1[j]][s1[i]]+matrizAdj[s1[i+1]][s1[j+1]]-matrizAdj[s1[j]][s1[j+1]];
            if (t<d){
                d=t;
                x=i+1;
                y=j+1;
            }
        }
    }

    if(d<0){
    
        reverse(s1.begin()+x,s1.begin()+y);

        custo1+=d;

        return 0;
    }

    return 1;
}

int Reinsertion(vector<int> &s1, double &custo1){
    
    double d=0,t=0,t0=0;
    int x,y;
    for (int i=1;i<dimension;i++){
        
        t0=matrizAdj[s1[i-1]][s1[i+1]]-matrizAdj[s1[i-1]][s1[i]]-matrizAdj[s1[i]][s1[i+1]];

        for (int j=0;j<dimension;j++){
            if (j>=i-2&&j<i+2) {
                j=i+1;
                continue;                
            }
            t=t0+matrizAdj[s1[i]][s1[j]]+matrizAdj[s1[i]][s1[j+1]]-matrizAdj[s1[j]][s1[j+1]];
            if (t<d){
                d=t;
                x=i;
                y=j;
            }
        }
    }
    
    if (d<0){
        int r=s1[x];
        if (x>y) y++;
    
        s1.erase(s1.begin()+x);
        s1.insert(s1.begin()+y,r);
        custo1+=d;
        
        return 0;
    }
    
    return 1;
}

int OrOpt2(vector<int> &s1, double &custo1){
    
    double d=0,t=0,t0=0;
    int x,y,r[2];
    for (int i=1;i<dimension-1;i++){
        
        t0=matrizAdj[s1[i-1]][s1[i+2]]-matrizAdj[s1[i]][s1[i-1]]-matrizAdj[s1[i+1]][s1[i+2]];

        for (int j=0;j<dimension;j++){
            if (j>=i-3&&j<i+3){
                j=i+2;
                continue;
            }
            t=t0+matrizAdj[s1[i]][s1[j]]+matrizAdj[s1[i+1]][s1[j+1]]-matrizAdj[s1[j]][s1[j+1]];
            if (t<d){
                d=t;
                x=i;
                y=j+1;
                
            }
        }
    }
    
    if (d<0){

        r[0]=s1[x];
        r[1]=s1[x+1];
        if (y-1>x) y-=2;
        
        s1.erase(s1.begin()+x,s1.begin()+x+2);
        s1.insert(s1.begin()+y,r,r+2);
        
        custo1+=d;

        return 0;
    }
    
    return 1;
}

int OrOpt3(vector<int> &s1, double &custo1){
    
    double d=0,t=0,t0=0;
    int x,y,r[3];
    for (int i=1;i<dimension-2;i++){
        
        t0=matrizAdj[s1[i-1]][s1[i+3]]-matrizAdj[s1[i-1]][s1[i]]-matrizAdj[s1[i+2]][s1[i+3]];

        for (int j=0;j<dimension;j++){
            if (j>=i-4&&j<=i+4){
                j=i+4;
                continue;
            }
            t=t0+matrizAdj[s1[i]][s1[j]]+matrizAdj[s1[i+2]][s1[j+1]]-matrizAdj[s1[j]][s1[j+1]];
            if (t<d){
                d=t;
                x=i;
                y=j+1;
            }
        }
    }
    
    if (d<0){

        r[0]=s1[x];
        r[1]=s1[x+1];
        r[2]=s1[x+2];
        if (y-1>x) y-=3;
        
        s1.erase(s1.begin()+x,s1.begin()+x+3);
        s1.insert(s1.begin()+y,r,r+3);
        
        custo1+=d;

        return 0;
    }
    
    return 1;
}

void rvnd(vector<int> &s1, double &custo1){
    
    vector<int> v={1,2,3,4,5};
    while (!v.empty()){ 
        int e=rand()%v.size(),c=1;

        switch(v[e]){
            case 1:
                c=Swap(s1,custo1);
                break;
            case 2:
                c=Reinsertion(s1,custo1);
                break;
            case 3:
                c=OrOpt2(s1,custo1);
                break;
            case 4:
                c=OrOpt3(s1,custo1);
                break;
            case 5:
                c=Opt2(s1,custo1);
                break;
        }

        if (c) v.erase(v.begin()+e);
        else v={1,2,3,4,5};
    }
}

void perturb(vector<int> &s1, double &custo1){
    
    int t=dimension/10;
    if (dimension<=30) t=3;
    int r[4];
    r[0]=rand()%(dimension-5);
    r[1]=rand()%(min(dimension-5-r[0],t-2))+r[0]+2;
    r[2]=rand()%(dimension-3-r[1])+r[1]+1;
    r[3]=rand()%(min(dimension-2-r[2],t-2))+r[2]+2;

    custo1+=matrizAdj[s1[r[0]]][s1[r[2]+1]]+matrizAdj[s1[r[1]]][s1[r[3]+1]]+matrizAdj[s1[r[2]]][s1[r[0]+1]]+matrizAdj[s1[r[3]]][s1[r[1]+1]]-matrizAdj[s1[r[0]]][s1[r[0]+1]]-matrizAdj[s1[r[1]]][s1[r[1]+1]]-matrizAdj[s1[r[2]]][s1[r[2]+1]]-matrizAdj[s1[r[3]]][s1[r[3]+1]];

    vector<int> sub1(s1.begin()+r[0]+1,s1.begin()+r[1]+1);
    vector<int> sub2(s1.begin()+r[2]+1,s1.begin()+r[3]+1);
    
    s1.erase(s1.begin()+r[0]+1,s1.begin()+r[1]+1);
    s1.erase(s1.begin()+r[2]+1-sub1.size(),s1.begin()+r[3]+1-sub1.size());
    s1.insert(s1.begin()+r[0]+1,sub2.begin(),sub2.end());
    s1.insert(s1.begin()+r[2]+1-sub1.size()+sub2.size(),sub1.begin(),sub1.end());

}

int main(int argc, char** argv)
{
	int imx=50,ils;
	double custoMin=numeric_limits<double>::max(),custo1=0,custo2=0;
	vector<int> bestS,s1,s2;
    bestS.reserve(600);
    s1.reserve(600);
    s2.reserve(600);
    srand (time(NULL));

    readData(argc, argv, &dimension, &matrizAdj);

    auto start = chrono::system_clock::now();

    if (dimension>=150) ils=dimension/2;
    else ils=dimension;

    for (int m=0;m<imx;m++){
        
        construction(s1,custo1);
    
        s2=s1;
        custo2=custo1;
        int n=0;
        while (n<ils){
            
            rvnd(s1,custo1);

            if (custo1<custo2){
                s2=s1;
                custo2=custo1;
                n=-1;
            }
            else {
                s1=s2;
                custo1=custo2;
            }
            
            perturb(s1,custo1);
            
            n++;
        }
        if (custo2<custoMin){
            custoMin=custo2;
            bestS=s2;
        }
    }
    auto end = chrono::system_clock::now();
    chrono::duration<double> time=end-start;

    /*printf("Solution: ");
    for (int i=0;i<dimension+1;i++){
        printf("%i ",bestS[i]);
    }
    printf("\n\n");*/

    printf("Tempo: %lf\nCusto: %lf\n\n",time,custoMin);
    
    return 0;
}
