/*
+----------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              ! Relatorio                                               !
+------------------+---------------------------------------------------------+
!Modulo            ! FIN                                                     !
+------------------+---------------------------------------------------------+
!Nome              ! RFIN001                                                 !
+------------------+---------------------------------------------------------+
!Descricao         ! Relatório de autorização de pagamento SINDAFEP.         !
!                  !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Marcos Vinicius Perusselo                               !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 28/01/2014                                              !
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
#INCLUDE "RWMAKE.CH"

#DEFINE CRLF CHR(13) + CHR(10)

User Function RFIN001()
	Local oPrint := Nil
	Local oFont10
	Local oFont14
	Local oFont18
	Local oFont24
	Local nRow := 0
	Local cFornece := ""
	Local cNota := ""
	Local cSerie := ""
	Local cDtEmiss := ""
	Local aDtVenc := {}
	Local nDtVLin := 0
	Local cVlPgto := ""
	Local cRefer := ""
	Local cVlParc := ""
	Local cEmpresa := ""
	Local cUsrLanc := ""
	Local cDataGer := ""
	Local cLogo := "\system\sindafep.bmp"
	Local aVencto := {}
	Local aValores:= {}
	Local aParcel := {}
	Local nTotal  := 0
	Local aAreaSE2 := SE2->(GetArea())
	Local aAreaSF1 := SF1->(GetArea())
	Local cAliasSE5 := GetNextAlias()
	Private oDlg
	Private aRefs := {}
	Private aTaxas := {} // Taxas
	
	oFont10  := TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont12  := TFont():New("Arial",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont12m := TFont():New("Courier New",9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont12mb:= TFont():New("Courier New",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14  := TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont14b := TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont18  := TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont24  := TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)
	
	oPrint:= TMSPrinter():New("Autorizacao Pagamento SINDAFEP")
	oPrint:Setup()
	oPrint:SetPortrait()
	oPrint:StartPage()
	
	nRow := 20
	
	// Chamada do SIGACOM - Doc. Entrada
	If (FunName() == "MATA103")
		dbSelectArea("SE2")
		SE2->(dbSetOrder(6))
		SE2->(dbGoTop())
		SE2->(dbSeek(xFilial("SE2")+SF1->(F1_FORNECE+F1_LOJA+F1_SERIE+F1_DOC)))
		
		While (SE2->(!Eof()) .And. SE2->(E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM) == xFilial("SE2")+SF1->(F1_FORNECE+F1_LOJA+F1_SERIE+F1_DOC))
			If (SE2->E2_TIPO <> 'TX ')
				// Adiciona Vencimento e Parcela ao Array de Parcelas
				aAdd(aParcel,{;
					SE2->E2_VENCTO,;
					SE2->E2_VALOR,;
					SE2->E2_HIST;
					})
				
				// Armazena o Total dos títulos
				nTotal += SE2->E2_VALOR
			EndIf
			SE2->(dbSkip())
		EndDo

		// Retorna a quantidade e a descrição dos produtos da NF
		RetDescr(xFilial("SF1")+SF1->(F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA))
		
		// Retorna Títulos de Taxas
		aTaxas := RetTaxas(xFilial("SE2")+SF1->F1_SERIE+SF1->F1_DOC)
		
		cFornece := AllTrim(Posicione("SA2", 1, xFilial("SA2")+SF1->(F1_FORNECE+F1_LOJA), "A2_NOME"))
		cNota := AllTrim(SF1->F1_DOC)
		cSerie := AllTrim(SF1->F1_SERIE)
		cDtEmiss := DtoC(SF1->F1_EMISSAO)
		cVlDoc := Transform(SF1->F1_VALBRUT, "@E 99,999,999,999,999.99")
		
	// Chamada do Contas a Pagar.
	ElseIf (FunName() == "FINA750")
		cChave := SE2->(E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM)
		
		nRecnoSE2 := SE2->(RecNo())
		dbSelectArea("SE2")
		SE2->(dbGoTop())
		SE2->(dbSetOrder(6))
		SE2->(dbSeek(cChave))
		
		While (SE2->(!Eof()) .And. SE2->(E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM) == cChave)
			If (SE2->E2_TIPO <> 'TX ')
				// Validacao para remover as parcelas de desdobramento.
				BeginSQL Alias cAliasSE5
					SELECT *
					FROM %Table:SE5%
					WHERE
					E5_FILIAL = %xFilial:SE5%
					AND E5_PREFIXO = %Exp:SE2->E2_PREFIXO%
					AND E5_NUMERO = %Exp:SE2->E2_NUM%
					AND E5_PARCELA = %Exp:SE2->E2_PARCELA%
					AND E5_CLIFOR = %Exp:SE2->E2_FORNECE%
					AND E5_LOJA = %Exp:SE2->E2_LOJA%
					AND %NotDel%
					AND E5_MOEDA = %Exp:Space(TamSX3("E5_MOEDA")[1])%
					AND (E5_MOTBX = 'DSD' OR E5_MOTBX = 'FAT')
				EndSQL
				
				If ((cAliasSE5)->(EOF()))
					// Adiciona Vencimento e Parcela ao Array de Parcelas
					aAdd(aParcel,{;
						SE2->E2_VENCTO,;
						SE2->E2_VALOR,;
						SE2->E2_HIST;
						})
					
					// Armazena o Total dos títulos
					nTotal += SE2->E2_VALOR
				EndIf
				
				(cAliasSE5)->(dbCloseArea())
			EndIf
			
			SE2->(dbSkip())
		EndDo

		SE2->(dbGoTo(nRecnoSE2))
		
		// Retorna Títulos de Taxas
		aTaxas := RetTaxas(xFilial("SE2")+SE2->E2_PREFIXO+SE2->E2_NUM)

		SE2->(dbGoTo(nRecnoSE2))
		
		cFornece 	:= AllTrim(Posicione("SA2", 1, xFilial("SA2")+SE2->(E2_FORNECE+E2_LOJA), "A2_NOME"))
		cNota 		:= AllTrim(SE2->E2_NUM)
		cSerie 		:= AllTrim(SE2->E2_PREFIXO)
		
		dDtEmiss 	:= POSICIONE("SF1",1,xFilial("SF1")+SE2->(E2_NUM+E2_PREFIXO+E2_FORNECE+E2_LOJA+E2_TIPO),"F1_EMISSAO")
		nVlDoc	 	:= POSICIONE("SF1",1,xFilial("SF1")+SE2->(E2_NUM+E2_PREFIXO+E2_FORNECE+E2_LOJA+E2_TIPO),"F1_VALBRUT")
		
		If AllTrim(DtoS(dDtEmiss)) == ''
			dDtEmiss := SE2->E2_EMISSAO
		EndIf
		
		If nVlDoc == 0
			nVlDoc := nTotal
		EndIf
		
		cDtEmiss 	:= DtoC(dDtEmiss)
		cVlDoc 		:= Transform(nVlDoc,"@E 99,999,999,999,999.99")
	EndIf
	
	/*
	nDtVLin := 4
	aDtVenc := {}
	nTotDt	:= 0
		
	For nX := 1 To Len(aVencto) Step nDtVLin
		
		cDtVenc := ''
		
		If (Len(aVencto) - nTotDt) > nDtVLin
			nDtRest := nDtVLin
		Else
			nDtRest := Len(aVencto) - nTotDt
		EndIf
		
		For nY := 0 To nDtRest - 1
		
			nTotDt++
			cDtVenc += DtoC(aVencto[nX + nY])
		
			If nY < nDtRest - 1
				cDtVenc += ", "
			EndIf
		Next nY
		aAdd(aDtVenc,cDtVenc)
	Next nX
	*/

	//cDtVenc += DtoC(aVencto[nX]) + IIF(nX != Len(aVencto), ", ", "")
	
	cEmpresa := SM0->M0_NOMECOM
	cUsrLanc := UsrFullName(RetCodUsr())
	cDataGer := DtoC(Date()) + " " + Time()
	
	oPrint:Say(nRow+80,0600,"AUTORIZAÇÃO DE PAGAMENTO",oFont24)
	
	If (File(cLogo))
		oPrint:SayBitmap(nRow+0030,90,cLogo,310,200)
	EndIf
	
	nRow += 50
	
	oPrint:Say(nRow+0230,0100,"Fornecedor: " + cFornece,oFont14b)
	oPrint:Say(nRow+0330,0100,"Documento: " + cNota + IIF(AllTrim(cSerie) != ''," / " + cSerie,''),oFont14b)
	oPrint:Say(nRow+0330,0900,"Data Emissão: " + cDtEmiss,oFont14b)
	oPrint:Say(nRow+0330,1700,"Valor: " + cVlDoc,oFont14b)
	
	cRefer := Space(TamSx3("E2_HIST")[1])
	
	If (Len(aRefs) == 0)
		@ 100,030 TO 300,700 DIALOG oDlg TITLE "Aviso"
//		@ 13, 10 SAY "Referencia dos titulos está em branco!"
		@ 13, 10 SAY "Referencia Personalizada:"
		@ 23, 10 SAY "Preencha com a referência personalizada que deve aparecer na Autoriz. de Pgto.!"
		@ 48, 10 GET cRefer Picture "@X" Size 70,10 Valid .T.
		ACTIVATE MSDIALOG oDlg ON INIT (EnchoiceBar(oDlg,{||(oDlg:End())},{||oDlg:End()})) CENTERED
		
		If AllTrim(cRefer) != ''
			aAdd(aRefs, cRefer)
		EndIf
	EndIf
	
	// Se houver referência personalizada
	If Len(aRefs) > 0
		oPrint:Line (nRow+0420,0045,nRow+420,2300)
		
		oPrint:Say(nRow+0470,0100,"REFERÊNCIA:",oFont14b)
	
		For nX := 1 To Len(aRefs)
			nRow += 70
			oPrint:Say(nRow+0470,0150,aRefs[nX],oFont14)
		Next nX	
		
		nRow += 100
	EndIf
	
	oPrint:Line (nRow+0500,0045,nRow+500,2300)
	nRow += 20
	
	oPrint:Say(nRow+0550,0100,"PARCELA(S): ",oFont14b)
	nRow += 120
	
	oPrint:Say(nRow+0550,0100,"Descrição",oFont14b)
	oPrint:Say(nRow+0550,1350,"Vencimento",oFont14b)
	oPrint:Say(nRow+0550,2100,"Valor",oFont14b)
	
	For nZ := 1 To Len(aParcel)
		//cDescParc:= IIF(AllTrim(aParcel[nZ][3]) != '' , aParcel[nZ][3] , IIF(Len(aRefs) > 0, aRefs[1],'') )
		cDescParc:= Substr(aParcel[nZ][3],1,40)
		cVencParc:= DtoC(aParcel[nZ][1])
		cVlParc	 := Transform(aParcel[nZ][2],"@E 99,999,999,999,999.99")
	
		nRow += 70 
		oPrint:Say(nRow+0550,0100,cDescParc,oFont14)
		oPrint:Say(nRow+0550,1350,cVencParc,oFont14)
		oPrint:Say(nRow+0550,1700,cVlParc,oFont12m)
	Next nZ
	
	nRow 		+= 100
	cTotalParc 	:= Transform(nTotal,"@E 99,999,999,999,999.99")

	oPrint:Say(nRow+550,1350,"TOTAL PARCELA(S):",oFont14b)
	oPrint:Say(nRow+550,1700,cTotalParc,oFont12mb)

	If Len(aTaxas) > 0
		nTotalTxs := 0
		
		nRow += 100
		oPrint:Line (nRow+0700,0045,nRow+700,2300)
		
		nRow += 70
		
		oPrint:Say(nRow+700,0100,"TAXAS:",oFont14b)
		nRow += 120
		oPrint:Say(nRow+700,0100,"Descrição",oFont14b)
		oPrint:Say(nRow+700,1350,"Vencimento",oFont14b)
		oPrint:Say(nRow+700,2100,"Valor",oFont14b)
		
		For nZ := 1 To Len(aTaxas)
			nTotalTxs += aTaxas[nZ][5]
			
			cDescrTx := aTaxas[nZ][2]
			cVenctoTx:= DtoC(aTaxas[nZ][4])
			cVlTx	 := Transform(aTaxas[nZ][5],"@E 99,999,999,999,999.99")
			
			nRow += 70
			oPrint:Say(nRow+700,0100,cDescrTx,oFont14)
			oPrint:Say(nRow+700,1350,cVenctoTx,oFont14)
			oPrint:Say(nRow+700,1700,cVlTx,oFont12m)
		Next nZ
		
		nRow 		+= 100
		cTotalTxs 	:= Transform(nTotalTxs,"@E 99,999,999,999,999.99")

		oPrint:Say(nRow+700,1350,"TOTAL TAXAS:",oFont14b)
		oPrint:Say(nRow+700,1700,cTotalTxs,oFont12mb)
		
		nRow		+= 100
	EndIf
	
	oPrint:Say(nRow+0750,0050,"Empresa:",oFont14)
	oPrint:Say(nRow+0825,0100,cEmpresa,oFont10)
	
	oPrint:Say(nRow+0750,1330,"Lançado por:",oFont14)
	oPrint:Say(nRow+0825,1380,cUsrLanc,oFont10)
	
	oPrint:Say(nRow+0930,0050,"Visto do conferente:",oFont14)
	
	oPrint:Say(nRow+0930,0630,"Visto da autorização:",oFont14)
	
	oPrint:Say(nRow+0930,1780,"Data:",oFont14)
	oPrint:Say(nRow+1000,1830,cDataGer,oFont10)
	
	oPrint:Line (nRow+0745,0045,nRow+0745,2300)
	oPrint:Line (nRow+0745,0045,nRow+1100,0045)
	oPrint:Line (nRow+1100,0045,nRow+1100,2300)
	oPrint:Line (nRow+0745,2300,nRow+1100,2300)
	
	oPrint:Line (nRow+0925,0045,nRow+0925,2300)
	oPrint:Line (nRow+0745,1325,nRow+0925,1325)
	oPrint:Line (nRow+0925,0625,nRow+1100,0625)
	oPrint:Line (nRow+0925,1775,nRow+1100,1775)
	
	oPrint:Say(nRow+1150,0045,"Sindafep - Sindicato dos Auditores Fiscais da Receita do Estado do Paraná",oFont10)
	
	oPrint:EndPage()
	oPrint:Preview()
	
	RestArea(aAreaSE2)
	RestArea(aAreaSF1)
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	!RetDescr   ! Autor !Gilson Lima 		 ! Data ! 			     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao auxiliar de processamento... 							 !
! 			! Retorna array com dados de ítens da NF						 !
+----------------------------------------------------------------------------*/
Static Function RetDescr(cChave)
	Local aArea := GetArea()
	
	dbSelectArea("SD1")
	
	SD1->(dbSetOrder(1))
	SD1->(dbGoTop())
	SD1->(dbSeek(cChave))
	
	While SD1->(!EOF()) .And. SD1->(D1_FILIAL+D1_DOC+D1_SERIE+D1_FORNECE+D1_LOJA) == cChave
		aAdd(aRefs,StrZero(SD1->D1_QUANT,6) + "   " + SD1->D1_UM + "   " + POSICIONE("SB1",1,xFilial("SB1")+SD1->D1_COD,"B1_DESC"))
	
		SD1->(dbSkip())
	EndDo
	
	SD1->(dbCloseArea())
	
	RestArea(aArea)
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	!RetTaxas   ! Autor !Gilson Lima 		 ! Data ! 20/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao auxiliar de processamento... 							 !
! 			! Retorna array com dados de taxas do título					 !
+----------------------------------------------------------------------------*/
Static Function RetTaxas(cDadosTit)
	Local aTxs 		:= {}
	Local aAreaSE2 	:= GetArea("SE2")
	Local aAreaSED	:= GetArea("SED")
	Local nIndice   := 17
	Local cTipo		:= 'TX '
	Local cChave	:= cDadosTit+cTipo
	
	dbSelectArea("SE2")
	SE2->(dbSetOrder(nIndice))
	SE2->(dbGoTop())
	SE2->(dbSeek(cChave))
	
	While SE2->(!EOF()) .And. SE2->(E2_FILIAL+E2_PREFIXO+E2_NUM+E2_TIPO) == cChave
		aAdd(aTxs,{;
			SE2->E2_NATUREZ,;
			POSICIONE("SED",1,XFILIAL("SED")+SE2->E2_NATUREZ,"ED_DESCRIC"),;
			SE2->E2_HIST,;
			SE2->E2_VENCTO,;
			SE2->E2_VALOR;
		})
		
		SE2->(dbSkip())
	End
	SE2->(dbCloseArea())
	
	// Restaura áreas
	RestArea(aAreaSE2)
	RestArea(aAreaSED)
Return (aTxs)