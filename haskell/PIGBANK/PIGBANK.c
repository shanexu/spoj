#include<stdio.h>
#define INF 1<<25

int min(int a,int b)
{
  return a<b?a:b;
}
int ff[10050];
int main()
{
  int T,p[550],w[550],e,f,pp,i,j,n;
  scanf("%d",&T);
  while(T--)
    {
      scanf("%d%d",&e,&f);
      pp=f-e;
      for(i=0;i<=pp;i++)
        ff[i]=INF;
      scanf("%d",&n);
      for(i=0;i<n;i++)
        scanf("%d%d",&p[i],&w[i]);
      ff[0]=0;
      for(i=0;i<n;i++)
        for(j=w[i];j<=pp;j++)
          ff[j]=min(ff[j],ff[j-w[i]]+p[i]);
      if(ff[pp]==INF) printf("This is impossible.\n");
      else printf("The minimum amount of money in the piggy-bank is %d.\n",ff[pp]);
    }
  return 0;
}
