*format "%10i"
*npoin
*loop nodes
*format "%10i%20.2f%20.2f"
*NodesNum*NodesCoord
*end nodes
*format "%10i"
*nelem
*loop elems
*format "%10i%10i%10i%10i"
*elemsnum*elemsConec(swap)*elemsmat 
*end elems
*format "%10i"
*nmats
*loop materials
*format "%10i%30.5f%15.5f"
*matnum() *MatProp(1) *MatProp(2)
*end materials
*Set Cond Cargas-Pontuais *nodes
*format "%10i"
*CondNumEntities(int)
*if(CondNumEntities(int)>0)
*loop nodes *OnlyInCond
*format "%10i%20.2f%20.2f"
*NodesNum*cond(1)*cond(2) 
*end nodes
*Set Cond Restricoes-Pontuais *nodes *or(1,int) *or(2,int)
*format "%10i"
*CondNumEntities(int)
*loop nodes *OnlyInCond
*format "%10i%10i%10i%10i%10i"
*NodesNum*cond(1,int)*cond(3,int)*cond(2,int)*cond(4,int) 
*end nodes