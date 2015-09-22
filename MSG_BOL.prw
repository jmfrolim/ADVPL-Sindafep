#include "protheus.ch"
// ---------------------------------------------------------------------------------------------------------------
//   Função Mensagens Boleto
// ---------------------------------------------------------------------------------------------------------------
User Function MsgBol()

Local cPref  := SE1->E1_PREFIXO 
Local cMens  := Space(40)   
Local cAuxM1 := SEE->EE_FORMEN1
Local cAuxM2 := SEE->EE_FORMEN2
Local cAuxM3 := SEE->EE_FOREXT1

   If cPref == "MSL"
      cMens:=cAuxM1+Space(40-Len(cAuxM1)) 
  
   Elseif cPref == "TXM"   
   cMens:=cAuxM2+Space(40-Len(cAuxM2))
  
   Elseif cPref == "PLS"    
   cMens:=cAuxM3+Space(40-Len(cAuxM3))
      
   EndIf
   
Return(cMens)





