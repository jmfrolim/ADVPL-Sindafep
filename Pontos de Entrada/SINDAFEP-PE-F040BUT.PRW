/*
+----------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              ! Relatorio                                               !
+------------------+---------------------------------------------------------+
!Modulo            ! FAT                                                     !
+------------------+---------------------------------------------------------+
!Nome              ! F040BUT                                                 !
+------------------+---------------------------------------------------------+
!Descricao         ! Ponto de Entrada acionado no menu Ações Relacionadas	 !
!				   ! dos títulos a Receber								     !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Gilson M. Lima				                             !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 26/01/2015                                              !
+------------------+---------------------------------------------------------+
!   ATUALIZACOES                                                             !
+-------------------------------------------+-----------+-----------+--------+
!   Descricao detalhada da atualizacao      !Nome do    ! Analista  !Data da !
!                                           !Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!                                           !           !           !        !
!                                           !           !           !        !
+-------------------------------------------+-----------+-----------+--------+
*/
#INCLUDE "PROTHEUS.CH"

User Function F040BUT()

	Local aButtons 	:= {} 
	Local cRecSer	:= ''
	Local cRecNr	:= ''
	Local cChave	:= SE1->E1_PREFIXO+SE1->E1_NUM+SE1->E1_PARCELA
	
	aArea := GetArea()
	
	dbSelectArea("SZR")
	SZR->(dbSetOrder(2))
	SZR->(dbSeek(xFilial("SZR")+cChave))
	
	While SZR->(!EOF()) .And. SZR->(ZR_TITPREF+ZR_TITNUM+ZR_TITPARC) == cChave
		If SZR->ZR_STATUS != '2'
			cRecSer:= SZR->ZR_SERIE
			cRecNr := SZR->ZR_NUM
		EndIF
		
		SZR->(dbSkip())
	End
	
	SZR->(dbCloseArea())
	
	RestArea(aArea)
	
	//|-------------------------------------------------------------------------|
	//| Somente deve adicionar o botão na EnchoiceBar se não for Inclusão       |
	//| e Analisa se já existe Recibo ou o Título foi baixado				    |
	//|-------------------------------------------------------------------------| 

	If !Inclui .And. AllTrim(cRecNr) == ''                                                      
		Aadd( aButtons,{"Emitir Recibo",{||U_RRECIB000(1)},"Emitir Recibo"} )
	Endif
	If !Inclui .And. AllTrim(cRecNr) != ''
		Aadd( aButtons,{"Visualizar Recibo",{||U_RRECIB000(2)},"Visualizar Recibo"} )
		
		If SE1->E1_STATUS != 'B'
			Aadd( aButtons,{"Excluir Recibo",{||U_RRECIB000(3)},"Excluir Recibo"} )
		EndIF
	EndIF
	
Return(aButtons)