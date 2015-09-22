/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manutenção 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Ativo Fixo												 !
+------------------+---------------------------------------------------------+
!Nome 			   ! SINDAFEP_ATIVOF_ATVALOR.PRW							 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para atualização de valores depreciação 		 !
!				   ! de ativo fixo											 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 10/09/2015												 !
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
! Programa 	! ATFATVL	! Autor !Gilson Lima 		 ! Data ! 10/09/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Ajustes de valores de ativo fixo								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function ATFATVL()

	Private cArquivo:= ""
	Private cPerg	:= "ATFATVL"
	//Private aPergs	:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
	
		PutSx1(cPerg,"01","Arquivo Importação","","","mv_ch1","C",99,00,00,"G","","DIR","","","mv_par01","","","","","","","","","","","","","","","","",{"Selecione o Arquivo"},{},{},"")
		//AADD(aPergs,{"Arquivo Importação","Arquivo Importação","Arquivo Importação","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		//AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a função de importação
	If MsgYesNo("Confirma Importação?","Atenção")
		Processa({||ATFATV1()})
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ATFATVL	! Autor !Gilson Lima 		 ! Data ! 10/09/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Ajustes de valores de ativo fixo								 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function ATFATV1()
	
	Local aArea	:= GetArea()
	
	Local cArquivo 	:= AllTrim(MV_PAR01)
	Local nHdl		:= 0
	Local nTamFile	:= 0
	Local nRestaLer := 0
	Local nFimLinha := 0
	Local nIniLeit	:= 0
	Local cLeitura	:= ''
	Local cString	:= ''
	Local cEol		:= CHR(13)+CHR(10)
	Local lContinua	:= .T.
	
	Local aDados	:= {}
	Local aArquivo	:= {}
	
	Local nImport	:= 0
	
	nHdl    := fOpen(cArquivo,68)
	
	If nHdl == -1
		MsgStop("O arquivo "+cArquivo+" não pode ser aberto... Verifique os parâmetros","Atencao")
	    Return(cArqDbf)
	Endif	
	
	nTamFile := nRestaLer := fSeek(nHdl,0,2)
	fSeek(nHdl,0,0)	
	
	While nRestaLer > 0
		
		aDados := {}
		
		cLeitura  := fReadStr(nHdl,65535)
	    nFimLinha := IIF(AT(cEol,cLeitura)==0, nRestaLer+1, AT(cEol,cLeitura)+1) 
		cString   := Substr(cLeitura,1,nFimLinha)
		If nFimLinha-1 <> nRestaLer
	    	cString   := Substr(cString,1,Len(cString)-2)
	    EndIf
		nRestaLer -= Len(cString)+2
		nIniLeit  += Len(cString)+2 
		
		fSeek(nHdl,nIniLeit)
			    
	    aDados := Separa(cString,'|',.T.)
	    
	    aAdd(aArquivo,aDados)
			
	EndDo
	
	fClose(nHdl)
	
	dbSelectArea('SN3')
	SN3->(dbSetOrder(1))
	
	For nA := 1 To Len(aArquivo)	
	
			SN3->(dbGoTop())
			If SN3->(dbSeek(aArquivo[nA][1]+PadR(aArquivo[nA][2],TamSX3("N3_CBASE")[1],' ')))
		
				RecLock('SN3',.F.)
				
					SN3->N3_VRDBAL1 := Val(aArquivo[nA][3])
					SN3->N3_VRDACM1 := Val(aArquivo[nA][4])
					SN3->N3_VMXDePR := Val(aArquivo[nA][5])
				
				MsUnLock()
			EndIf
	
	Next nA
	
	SN3->(dbCloseArea())
	
	RestArea(aArea)
	
	Alert('Atualização Valores Concluída!')

Return