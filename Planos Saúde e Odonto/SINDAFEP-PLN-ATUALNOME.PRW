/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manutenção 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro												 !
+------------------+---------------------------------------------------------+
!Nome 			   ! FIN-PLANOS-ATUALNOME 									 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a Aglutinação do Contas a Receber dos Planos!
!				   ! de Saúde o Odontológicos								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 06/04/2015												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 											! 			! 			! 		 !
! 											! 			! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/

#Include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PlnAtNm	! Autor !Gilson Lima 		 ! Data ! 13/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao atualiza nomes na tabela SZJ, baseada no Código de 	 !
!           ! parentesco e Loja. 													 !
+----------------------------------------------------------------------------*/
User Function PlnAtNm()

	Local cNome := ''
	Local nRegL	:= 0
	Local nRegA := 0
	
	dbSelectArea("SZJ")
	SZJ->(dbSetOrder(1))
	SZJ->(dbGoTop())
	
	While SZJ->(!EOF())
		
		If SZJ->ZJ_CODPAR == '000000'
			cNome := POSICIONE("SA1",1,xFilial("SA1")+SZJ->ZJ_CODIGO+SZJ->ZJ_LOJA,"A1_NOME")
		Else
			cNome := POSICIONE("SZG",1,xFilial("SZG")+SZJ->ZJ_CODIGO,"ZG_NOME")
		EndIf
		
		nRegL++
		
		If AllTrim(cNome) != ''
		
			RecLock("SZJ",.F.)
			
				SZJ->ZJ_NOME := cNome
				
			MsUnLock()
			
			nRegA++
			
		EndIf
		
		SZJ->(dbSkip())
	End
	
	Alert("Concluído: Foram lidos " + cValToChar(nRegL) + " registros na SZJ e efetuada " + cValToChar(nRegA) + " alterações.")

Return