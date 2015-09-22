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
!Nome 			   ! CUST-FISCAIS 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a manutenção de filiação x desfiliação dos	 !
!				   ! fiscais no cadastro de clientes								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 23/12/2014												 !
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
! Programa 	! FALTFIL	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Manutenção de Filiação dos fiscais					 		 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User function FAltFil()

	Private cPerg		:= "FALTFIL"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Ação","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","Filiação","","","","","Desfiliação","","","","","","","","","","","","","","","","","","","","","","",""})
		//AADD(aPergs,{"Ação","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","Filiação","","","","","Desfiliação","","","","","Suspensão","","","","","Exclusão Provisória","","","","","Retorno","","","","","","","",""})
		AADD(aPergs,{"Data","","","mv_ch2","D",8,0,0,"G","NAOVAZIO()","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Motivo","","","mv_ch3","C",50,0,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)

	//Chama função geração de títulos
	If MsgYesNo("Confirma a manutenção de filiação do fiscal?","Atenção")
		Processa({||U_MFilFisc()}) 
	Endif 
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! MFilFisc	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Manutenção de Filiação dos fiscais					 		 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function MFilFisc()

	Local dData	 := CtoD(StrZero(Day(Date()),2) +"/"+ StrZero(Month(Date()),2) +"/"+ cValToChar(Year(Date())))
	Local aDados := {}
	
	// Dados para gravação na tabela de histórico
	aAdd(aDados,SA1->A1_FILIAL)			// Filial do Fiscal
	aAdd(aDados,SA1->A1_LOJA)			// Loja do Fiscal
	aAdd(aDados,SA1->A1_COD)			// Código do Fiscal
	aAdd(aDados,cValToChar(MV_PAR01))	// Tipo de Ação [1 = Filiação | 2 = Desfiliação | 3 = Suspensão | 4 = Exclusão Prov. | 5 = Retorno]
	aAdd(aDados,MV_PAR02)				// Data da Ocorrência
	aAdd(aDados,MV_PAR03)				// Motivo
	aAdd(aDados,dData)					// Data da Manutenção
	aAdd(aDados,Time())					// Hora da Manutenção
	aAdd(aDados,__CUSERID)				// Cód. Usuário que efetuou a manutenção
	aAdd(aDados,CUSERNAME)				// Nome Usuário que efetuou a manutenção
	
	If Alltrim(DtoS(MV_PAR02)) != ''
		Do Case
			Case MV_PAR01 == 1	// Filiação
				
				// Atualiza SA1
				RecLock("SA1", .F.)
					SA1->A1_XDTFIL 	:= MV_PAR02
					SA1->A1_XDTDESF := StoD('')
					SA1->A1_XFILNSI := 'S'
				MsUnlock()
				
				// Atualiza Variáveis na Memória
				M->A1_XDTFIL 	:= MV_PAR02
				M->A1_XDTDESF 	:= StoD('')
				M->A1_XFILNSI := 'S'
				
				// Adiciona a Ação na descrição
				aDados[6] := Substr("1 | Filiação | " + MV_PAR03, 1, 50) 				
				
				// Grava o Histórico
				GrHsFisc(aDados)
				
			Case MV_PAR01 == 2	// Desfiliação

				// Atualiza SA1			
				RecLock("SA1", .F.)
					SA1->A1_XDTDESF	:= MV_PAR02
					SA1->A1_XFILNSI := 'N'
				MsUnLock()

				// Atualiza Variáveis na Memória
				M->A1_XDTDESF	:= MV_PAR02
				M->A1_XFILNSI 	:= 'N'

				// Adiciona a Ação na descrição
				aDados[6] := Substr("2 | Desfiliação | " + MV_PAR03, 1, 50) 				

				// Grava o Histórico
				GrHsFisc(aDados)
								
			Case MV_PAR01 == 3	// Suspensão

				// Atualiza SA1			
				RecLock("SA1", .F.)
					SA1->A1_XDTDESF	:= MV_PAR02
					SA1->A1_XFILNSI := 'N'
				MsUnLock()

				// Atualiza Variáveis na Memória
				M->A1_XDTDESF	:= MV_PAR02
				M->A1_XFILNSI 	:= 'N'

				// Adiciona a Ação na descrição
				aDados[6] := Substr("3 | Suspensão | " + MV_PAR03, 1, 50) 				

				// Grava o Histórico
				GrHsFisc(aDados)
					
			Case MV_PAR01 == 4	// Exclusão Provisória

				// Atualiza SA1			
				RecLock("SA1", .F.)
					SA1->A1_XDTDESF	:= MV_PAR02
					SA1->A1_XFILNSI := 'N'
				MsUnLock()

				// Atualiza Variáveis na Memória
				M->A1_XDTDESF	:= MV_PAR02
				M->A1_XFILNSI 	:= 'N'

				// Adiciona a Ação na descrição
				aDados[6] := Substr("4 | Excl. Prov. | " + MV_PAR03, 1, 50) 				

				// Grava o Histórico
				GrHsFisc(aDados)
					
			Case MV_PAR01 == 5	// Retorno
				
				// Atualiza SA1
				RecLock("SA1", .F.)
					SA1->A1_XDTFIL 	:= MV_PAR02
					SA1->A1_XDTDESF := StoD('')
					SA1->A1_XFILNSI := 'S'
				MsUnlock()
				
				// Atualiza Variáveis na Memória
				M->A1_XDTFIL 	:= MV_PAR02
				M->A1_XDTDESF 	:= StoD('')		
				M->A1_XFILNSI 	:= 'S'		

				// Adiciona a Ação na descrição
				aDados[6] := Substr("5 | Retorno | " + MV_PAR03, 1, 50) 				
				
				// Grava o Histórico
				GrHsFisc(aDados)			
				
		EndCase
		
		MsgInfo("Filiação alterada com sucesso!", "Atenção")
	EndIf
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GRHSFISC	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Grava o historico de filiacao do Fiscal				 		 !
! 			! 																 !
+----------------------------------------------------------------------------*/
Static Function GrHsFisc(aDados)

	Local aArea := GetArea()

	dbSelectArea("SZM")
	RecLock("SZM", .T.)
		
		SZM->ZM_FILIAL	:= xFilial("SZM")
		SZM->ZM_FILFISC := aDados[1]
		SZM->ZM_LOJFISC := aDados[2]
		SZM->ZM_CODFISC := aDados[3]
		SZM->ZM_ACAO	:= aDados[4]
		SZM->ZM_DATA    := aDados[5]
		SZM->ZM_HISTOR  := aDados[6]
		SZM->ZM_DTMANUT := aDados[7]
		SZM->ZM_HRMANUT := aDados[8]
		SZM->ZM_USMANUT := aDados[9]
		SZM->ZM_NMMANUT := aDados[10]
	
	SZM->(MSUnLock())
	SZM->(dbCloseArea())
	
	RestArea(aArea)
Return


/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ATVALFIS	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcão temporário para atualização de valores fiscais			 !
! 			! baseada no arquivo 5MH (Tabela SZY)							 !
+----------------------------------------------------------------------------*/
User Function AtValFis()
	
	dbSelectArea("SZY")
	SZY->(dbSetOrder(1))
	SZY->(dbGoTop())
	
	While SZY->(!EOF())
		
		dbSelectArea("SA1")
		SA1->(dbSetOrder(3))
		SA1->(dbGoTop())
		
		SA1->(dbSeek(xFilial("SA1")+PadR(Substr(SZY->ZY_CPF,3), 14, ' ')))
		
		If SA1->A1_CGC == PadR(Substr(SZY->ZY_CPF,3), 14, ' ')
			RecLock("SA1",.F.)
				SA1->A1_VLRMENS := SZY->ZY_VALOR
			SA1->(MsUnLock())
		EndIf

		SZY->(dbSkip())
	End
	
	alert("valores adicionados com sucesso")
Return

User Function ExbHFil()

	Local   cQuery 	:= ''
	Local	aFixe	:= {}
	
	Local	aCampos	:= {}
	Local	aInd	:= {}
	
	Private aCols 	:= {}
	Private aHeader	:= {}
	
	aSX3Data := TamSX3('ZM_DATA')
	aSX3Hist := TamSX3('ZM_HISTOR')
	
	aAdd(aCampos,{"ZM_DATA",aSX3Data[3],aSX3Data[1],aSX3Data[2]})
	aAdd(aCampos,{"ZM_HISTOR",aSX3Hist[3],aSX3Hist[1],aSX3Hist[2]})

	aAdd(aInd,{CriaTrab(Nil,.F.),"ZM_DATA","Data"})
	
	cArqTrab	:= CriaTrab(aCampos,.T.)
	
	dbUseArea(.T.,"DBFCDX",cArqTrab,"ARQTMP",.T.,.F.)
	
	dbSelectArea("ARQTMP") 
	
	//Após selecionada a área do arquivo temporário... 
	For nA  := 1 to Len(aInd)                        
	    //Cria os índices utiliando o comando IndRegua 
	    IndRegua("ARQTMP",aInd[nA,1],aInd[nA,2],,,OemToAnsi("Criando Índice Temporário...")) 
	Next nA
		
	cQuery := "SELECT SZM.ZM_DATA, SZM.ZM_HISTOR, SZM.ZM_ACAO"
	cQuery += " FROM " + RetSqlName('SZM') + " SZM"
	cQuery += " WHERE SZM.D_E_L_E_T_ <> '*'"
	cQuery += " AND SZM.ZM_FILIAL = '" + xFilial('SZM') + "'"
	cQuery += " AND SZM.ZM_FILFISC = '" + SA1->A1_FILIAL + "'"
	cQuery += " AND SZM.ZM_LOJFISC = '" + SA1->A1_LOJA + "'"
	cQuery += " AND SZM.ZM_CODFISC = '" + SA1->A1_COD + "'"
	cQuery += " ORDER BY SZM.ZM_DATA"
	
	If Select('TRB') <> 0
		dbSelectArea('TRB')
		TRB->(dbCloseArea())
	EndIf
	
	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB',.F.,.F.)
	
	dbSelectArea('TRB')
	TRB->(dbGoTop())

	While TRB->(!EOF())
		
		RecLock("ARQTMP",.T.)
		
		ARQTMP->ZM_DATA   := StoD(TRB->ZM_DATA)
		ARQTMP->ZM_HISTOR := TRB->ZM_HISTOR
		
		ARQTMP->(MsUnlock())
		
		TRB->(dbSkip())
	End
	
	TRB->(dbCloseArea())
	
	ARQTMP->(dbGoTop())

	DEFINE DIALOG oDlg TITLE AllTrim(SA1->A1_NOME) + " :: Histórico de Filiação" FROM 180,180 TO 550,700 PIXEL
		                 
	    DbSelectArea('ARQTMP')

	    oBrowse := BrGetDDB():New( 1,1,260,156,,,,oDlg,,,,,,,,,,,,.F.,'ARQTMP',.T.,,.F.,,, )

	    oBrowse:AddColumn(TCColumn():New('Data'  	 ,{||ARQTMP->ZM_DATA } ,,,,'LEFT',,.F.,.F.,,,,.F.,))
	    oBrowse:AddColumn(TCColumn():New('Descrição' ,{||ARQTMP->ZM_HISTOR},,,,'LEFT',,.F.,.F.,,,,.F.,))
	
	    TButton():New( 172, 210, "Fechar", oDlg,{|| oDlg:End() },40,010,,,.F.,.T.,.F.,,.F.,,,.F. )
	    
	ACTIVATE DIALOG oDlg CENTERED 
	
	ARQTMP->(dbCloseArea())
	
Return
