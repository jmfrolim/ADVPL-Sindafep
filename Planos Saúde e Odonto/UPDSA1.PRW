#INCLUDE "PROTHEUS.CH"

USER FUNCTION UPDSA1()

	dbSelectArea("SA1")
	SA1->(dbSetOrder(1))
	SA1->(dbGoTop())
	
	While SA1->(!EOF())
	
		RecLock("SA1",.F.)
		
			SA1->A1_ZFCOBPL := SA1->A1_FORMCOB
			
		SA1->(MsUnlock())
	
		SA1->(dbSkip())
	End

	ALERT("ATUALIZAÇÃO EFETUADA COM SUCESSO!")
RETURN