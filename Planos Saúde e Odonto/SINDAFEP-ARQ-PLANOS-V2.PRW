/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Atualização 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro 											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! PLANOS00 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Importação do arquivo com dados de arquivos DBF da		 !
!				   ! versão anterior								 		 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 09/02/15 												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 									 		! 		 	! 		 	!		 !
! 											! 		 	! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/
#include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PLANOS00	! Autor !Gilson Lima 		 ! Data ! 09/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a importação do arquivo DBF SZH	 !
! 			! com dados dos titulares, planos e adicionais					 !
+----------------------------------------------------------------------------*/
User Function PLANOS00()

	Local _cAliasSZH
	Local _cCaminho
	
	_cAliasSZH := GetNextAlias ()
	_cCaminho := "\SINDAFEP\szh9901-bkp-v1.dbf"
	dbUseArea(.T.,,_cCaminho,_cAliasSZH,.F.)
	
	While (_cAliasSZH)->(!EOF())
	
		//Adiciona Plano principal à tabela de Integrantes SZJ
		
		RecLock("SZJ",.T.)
		
			SZJ->ZJ_FILIAL		:= (_cAliasSZH)->ZH_FILIAL
			SZJ->ZJ_FAMILIA		:= (_cAliasSZH)->ZH_FAMILIA
			SZJ->ZJ_CODPAR		:= '000000'					// Titular
			SZJ->ZJ_CODIGO		:= (_cAliasSZH)->ZH_CODTIT
			SZJ->ZJ_LOJA		:= (_cAliasSZH)->ZH_LOJA
			SZJ->ZJ_CODPLAN		:= (_cAliasSZH)->ZH_CODPLAN
			SZJ->ZJ_CARTEIR		:= (_cAliasSZH)->ZH_CARTEIR
			SZJ->ZJ_DTNASC		:= (_cAliasSZH)->ZH_DTNASC
			SZJ->ZJ_DATAINI		:= (_cAliasSZH)->ZH_DATAINI
			SZJ->ZJ_INC24H		:= (_cAliasSZH)->ZH_INC24H
			SZJ->ZJ_24HCOB		:= (_cAliasSZH)->ZH_24HCOB
			SZJ->ZJ_DATATER		:= (_cAliasSZH)->ZH_DATATER
			SZJ->ZJ_OBS			:= (_cAliasSZH)->ZH_OBS
		
		SZJ->(MsUnLock())
		
		// Verifica se Titular Utiliza SOS
		If (_cAliasSZH)->ZH_SOS == 'S'
		
			RecLock("SZJ",.T.)
			
				SZJ->ZJ_FILIAL		:= (_cAliasSZH)->ZH_FILIAL
				SZJ->ZJ_FAMILIA		:= (_cAliasSZH)->ZH_FAMILIA
				SZJ->ZJ_CODPAR		:= '000000'					// Titular
				SZJ->ZJ_CODIGO		:= (_cAliasSZH)->ZH_CODTIT
				SZJ->ZJ_LOJA		:= (_cAliasSZH)->ZH_LOJA
				SZJ->ZJ_CODADIC		:= '000001'					// SOS
				SZJ->ZJ_DTNASC		:= (_cAliasSZH)->ZH_DTNASC
				SZJ->ZJ_DATAINI		:= (_cAliasSZH)->ZH_DATAINI
				SZJ->ZJ_DATATER		:= (_cAliasSZH)->ZH_DATATER
			
			SZJ->(MsUnLock())
		
		EndIf

		// Verifica se Titular Utiliza AERO
		If (_cAliasSZH)->ZH_AERO == 'S'
		
			RecLock("SZJ",.T.)
			
				SZJ->ZJ_FILIAL		:= (_cAliasSZH)->ZH_FILIAL
				SZJ->ZJ_FAMILIA		:= (_cAliasSZH)->ZH_FAMILIA
				SZJ->ZJ_CODPAR		:= '000000'					// Titular
				SZJ->ZJ_CODIGO		:= (_cAliasSZH)->ZH_CODTIT
				SZJ->ZJ_LOJA		:= (_cAliasSZH)->ZH_LOJA
				SZJ->ZJ_CODADIC		:= '000002'					// AERO
				SZJ->ZJ_DTNASC		:= (_cAliasSZH)->ZH_DTNASC
				SZJ->ZJ_DATAINI		:= (_cAliasSZH)->ZH_DATAINI
				SZJ->ZJ_DATATER		:= (_cAliasSZH)->ZH_DATATER
			
			SZJ->(MsUnLock())
		
		EndIf
	
		(_cAliasSZH)->(dbSkip())
	EndDo
	 
	(_cAliasSZH)->(dbCloseArea())
	
	Alert("Importação realizada com sucesso!")

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PLANOS01	! Autor !Gilson Lima 		 ! Data ! 09/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a importação do arquivo DBF SZJ	 !
! 			! com dados dos dependentes e adicionais						 !
+----------------------------------------------------------------------------*/
User Function PLANOS01()

	Local _cAliasSZJ
	Local _cCaminho
	
	_cAliasSZJ := GetNextAlias ()
	_cCaminho2 := "\SINDAFEP\szj9901-bkp-v1.dbf"
	dbUseArea(.T.,,_cCaminho2,_cAliasSZJ,.F.)
	
	While (_cAliasSZJ)->(!EOF())
	
		// Verifica se Dependente Utiliza SOS
		If (_cAliasSZJ)->ZJ_SOS == 'S'
		
			RecLock("SZJ",.T.)
			
				SZJ->ZJ_FILIAL		:= (_cAliasSZJ)->ZJ_FILIAL
				SZJ->ZJ_FAMILIA		:= (_cAliasSZJ)->ZJ_FAMILIA
				SZJ->ZJ_CODPAR		:= (_cAliasSZJ)->ZJ_CODPAR
				SZJ->ZJ_CODIGO		:= (_cAliasSZJ)->ZJ_CODIGO
				SZJ->ZJ_CODADIC		:= '000001'					// SOS
				SZJ->ZJ_DTNASC		:= (_cAliasSZJ)->ZJ_DTNASC
				SZJ->ZJ_DATAINI		:= (_cAliasSZJ)->ZJ_DATAINI
				SZJ->ZJ_DATATER		:= (_cAliasSZJ)->ZJ_DATATER
			
			SZJ->(MsUnLock())
		
		EndIf

		// Verifica se Dependente Utiliza AERO
		If (_cAliasSZJ)->ZJ_AERO == 'S'
		
			RecLock("SZJ",.T.)
			
				SZJ->ZJ_FILIAL		:= (_cAliasSZJ)->ZJ_FILIAL
				SZJ->ZJ_FAMILIA		:= (_cAliasSZJ)->ZJ_FAMILIA
				SZJ->ZJ_CODPAR		:= (_cAliasSZJ)->ZJ_CODPAR
				SZJ->ZJ_CODIGO		:= (_cAliasSZJ)->ZJ_CODIGO
				SZJ->ZJ_CODADIC		:= '000002'					// AERO
				SZJ->ZJ_DTNASC		:= (_cAliasSZJ)->ZJ_DTNASC
				SZJ->ZJ_DATAINI		:= (_cAliasSZJ)->ZJ_DATAINI
				SZJ->ZJ_DATATER		:= (_cAliasSZJ)->ZJ_DATATER
			
			SZJ->(MsUnLock())
		
		EndIf
	
		(_cAliasSZJ)->(dbSkip())
	EndDo
	 
	(_cAliasSZJ)->(dbCloseArea())
	
	Alert("Importação realizada com sucesso!")

Return