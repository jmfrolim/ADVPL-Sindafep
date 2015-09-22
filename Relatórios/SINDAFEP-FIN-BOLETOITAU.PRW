#INCLUDE "RWMAKE.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "FONT.CH"
#INCLUDE "PROTHEUS.CH"

/*---------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              ! Relatório                                               !
+------------------+---------------------------------------------------------+
!Modulo            ! FIN - Financeiro		                                 !
+------------------+---------------------------------------------------------+
!Nome              ! RFIN003                                                 !
+------------------+---------------------------------------------------------+
!Descricao         ! Boleto Itau					                         !
+------------------+---------------------------------------------------------+
!Autor             ! Marcos Vinicius Perusselo                               !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 05/05/2015                                              !
+-------------------------------------------+-----------+-----------+--------+
!   Descricao detalhada da atualizacao      !Nome do    ! Analista  !Data da !
!                                           !Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!											!			!			!		 !
+-------------------------------------------+-----------+-----------+-------*/

#DEFINE RFIN001_CB "341"

User Function RFIN003()
	Local   nOpc       := 0
	Local   aDesc      := {"Este programa imprime os boletos de", "cobranca bancaria de acordo com", "os parametros informados"}
	PRIVATE Exec       := .F.
	Private cIndexName := ''
	Private cIndexKey  := ''
	Private cFilter    := ''
	Private cPerg := "ITAU      "
	Private lAbortPrint  := .F.
	
	DbSelectArea("SE1")
	SE1->(DBSetOrder(1))
	
	ValidPerg()
	Pergunte (cPerg, .F.)
	
	@ 96,42 TO 323,505 DIALOG oDlg5 TITLE "Impressao de Boletos"
	@ 02,02 SAY "Este programa tem como objetivo imprimir os boletos do"
	@ 03,02 SAY "Financeiro."
	@ 04,02 SAY ""
	@ 01,01 TO 5,25

	@ 91,089 BMPBUTTON TYPE 5 ACTION Pergunte(cPerg)
	@ 91,118 BMPBUTTON TYPE 1 ACTION OKProc()
	@ 91,146 BMPBUTTON TYPE 2 ACTION Close(oDlg5)
	ACTIVATE DIALOG oDlg5 Centered
Return

/*----------------------------------------------------------------------+
|Funcao:	|	OkProc		              								|
+-----------------------------------------------------------------------+
|Descricao:	|	Ok - Dados	Selecionados							  	|
+----------------------------------------------------------------------*/
Static Function OkProc()
	Pegadados()
	
	DbSelectArea("TRA")
	TRA->(DbGoTop())
	
	SelSe1()
Return Nil

/*----------------------------------------------------------------------+
|Funcao:	|	MarcDesm            									|
+-----------------------------------------------------------------------+
|Descricao:	|	INVERTE SELECAO    									  	|
+----------------------------------------------------------------------*/
Static Function MarcDesm(oDlg)
	DbSelectArea("TRA")
	TRA->(DBGotop())
	
	While !TRA->(EOF())
		If !Empty(TRA->OK)
			RECLOCK("TRA",.F.)
			TRA->OK := ""
			MsUnlock()
		Else
			RECLOCK("TRA",.F.)
			TRA->OK := GetMark()
			MsUnlock()
		EndIf
		
		TRA->(DbSkip())
	EndDo
	
	DlgRefresh(oDlg)
	DbGoTop()
Return

/*----------------------------------------------------------------------+
|Funcao:	|	PegaDados              									|
+-----------------------------------------------------------------------+
|Descricao:	|	Consulta de Dados									  	|
+----------------------------------------------------------------------*/
Static Function PegaDados()
	If (Select("TRA") <> 0)
		DbSelectArea("TRA")
		TRA->(dbCloseArea())
	EndIf

	cQry:= " SELECT * FROM " + RetSqlName("SE1")
	cQry+= " WHERE E1_PREFIXO BETWEEN '" + MV_PAR01 + "' AND '" + MV_PAR02 + "' "
	cQry+= " AND E1_NUM BETWEEN '" + MV_PAR03 + "' AND '" + MV_PAR04 + "' "
	cQry+= " AND E1_FILIAL      = '" + xFilial("SE1") + "' "
	cQry+= " AND RTRIM(E1_TIPO) IN ('5MH') "
	
	If !Empty(MV_PAR09)
		cQry+=" AND E1_EMISSAO >= '"+DtoS(MV_PAR09)+"' "
		cQry+=" AND E1_EMISSAO <= '"+DtoS(MV_PAR10)+"' "
	EndIf
	
	cQry+=" AND D_E_L_E_T_ = ' ' "
	cQry+=" ORDER BY E1_NUM, E1_PARCELA "
Return cQry

/*----------------------------------------------------------------------+
|Funcao:	|	FiltraBol              									|
+-----------------------------------------------------------------------+
|Descricao:	|	CHAMA ROTINA									  		|
+----------------------------------------------------------------------*/
Static Function FiltraBol(oDlg)
	Processa({|lEnd| MontaRel()})
	
	DbSelectArea("SE1")
	Set Filter to
	DbCloseArea()
	Delete File (cIndexName + ".IDX")
	Close(oDlg)
	Close(oDlg5)
Return

/*----------------------------------------------------------------------+
|Funcao:	|	SelSE1             										|
+-----------------------------------------------------------------------+
|Descricao:	|															|
+----------------------------------------------------------------------*/
Static Function SelSE1
	Local aEstr := SE1->(DbStruct())
	Private cMarca := GetMark(), lInverte := .F.
	
	aCampos := {}
	
	cQyr := PegaDados()
	Aadd(aEstr, {"OK","C",2,0})
	Aadd(aEstr, {"RECNO","N",10,0})
	
	cNomeArq := CriaTrab(aEstr)
	
	dbUseArea(.T.,__LocalDriver,cNomeArq,'TRA',.F.)
	Processa({||SqlToTrb(cQyr, aEstr, "TRA")})
	IndRegua ('TRA',cNomeArq,"TRA->E1_FILIAL+TRA->E1_NUM+TRA->E1_PARCELA",,,"Selecionando Registros...")
	
	aCampos := {}
	AADD(aCampos,{"OK",""," "})
	AADD(aCampos,{"E1_NUM","","Titulo"})
	AADD(aCampos,{"E1_PARCELA","","Parcela"})
	AADD(aCampos,{"E1_EMISSAO","","Emissao"})
	AADD(aCampos,{"E1_VENCREA","","Vencimento"})
	AADD(aCampos,{"E1_CLIENTE","","Cliente"})
	AADD(aCampos,{"E1_LOJA","","Loja"})
	
	TRA->(DbSelectArea('TRA'))
	TRA->(DbGoTop())
	@ 001,001 TO 400,700 DIALOG oDlg TITLE "Selecao de Titulos"
	
	oMark := MsSelect():New("TRA","OK",,aCampos,@lInverte,@cMarca,{18,3,125,350})
	oMark:oBrowse:bHeaderClick := {|oBrw,nCol| marcdesm(oDlg),oMark:oBrowse:SetFocus(),oMark:oBrowse:Refresh()}
	AddColMark(oMark,"OK")
	@ 15,30 BUTTON "Imprimir" SIZE 33,10 ACTION (Exec := .T.,filtrabol(oDlg))
	@ 15,40 BUTTON "Cancelar" SIZE 33,10 ACTION (Exec := .F.,Close(oDlg))
	@ 15,50 BUTTON "Selec./Deselec. Todos"  SIZE 65,10 ACTION marcdesm(oDlg)
	ACTIVATE DIALOG oDlg CENTERED
Return

/*----------------------------------------------------------------------+
|Funcao:	|	NNItau()            									|
+-----------------------------------------------------------------------+
|Descricao:	|	Nosso Numero Itau  									  	|
+----------------------------------------------------------------------*/
Static Function NNItau(cNTit)
	Local nNum     := '0'
	Local aAreaSE1 := SE1->(GetArea())
	Local aAreaSEE := SEE->(GetArea())
	
	SEE->(DbSetOrder(1))
	If SEE->(MSSeek(xFilial("SEE")+MV_PAR05+MV_PAR06+MV_PAR07+MV_PAR08))
		nNum := StrZero(Val(SEE->EE_FAXATU)+1,8)
		
		RECLOCK("SEE",.F.)
		SEE->EE_FAXATU := nNum
		SEE->(msUnlock())
	EndIf
	
	SE1->(DbSetOrder(1))
	If SE1->(MSSeek(xFilial("SE1")+cNTit))
		RECLOCK("SE1",.F.)
		SE1->E1_NUMBCO := nNum
		SE1->(msUnlock())
	EndIf
	
	RestArea(aAreaSE1)
	RestArea(aAreaSEE)
Return StrZero(Val(nNum), 8)

/*----------------------------------------------------------------------+
|Funcao:	|	MontaRel              									|
+-----------------------------------------------------------------------+
|Descricao:	|	Boleto												  	|
+----------------------------------------------------------------------*/
Static Function MontaRel()
	Local oPrint
	Local n := 0
	Local aDadosEmp := {}
	Local aDadosTit := {}
	Local aDadosBanco:= {}
	Local aDatSacado := {}
	Local i         := 1
	Local CB_RN_NN  := {}
	Local nRec      := 0
	Local _nVlrAbat := 0
	Local lPrint    := .F.
	Private xDac    := ""
	
	aDadosEmp := {	SM0->M0_NOMECOM                                           	,; //Nome da Empresa
	SM0->M0_ENDCOB                                            					,; //Endereço
	AllTrim(SM0->M0_BAIRCOB)+", "+AllTrim(SM0->M0_CIDCOB)+", "+SM0->M0_ESTCOB 	,; //Complemento
	"CEP: "+Subs(SM0->M0_CEPCOB,1,5)+"-"+Subs(SM0->M0_CEPCOB,6,3)             	,; //CEP
	"PABX/FAX: "+SM0->M0_TEL                                                  	,; //Telefones
	Subs(SM0->M0_CGC,1,2)+"."+Subs(SM0->M0_CGC,3,3)+"."+                       	;
		Subs(SM0->M0_CGC,6,3)+"/"+Subs(SM0->M0_CGC,9,4)+"-"+                    ;
		Subs(SM0->M0_CGC,13,2)                                                  ,; //CGC
	"I.E.: "+Subs(SM0->M0_INSC,1,3)+"."+Subs(SM0->M0_INSC,4,3)+"."+            	;
		Subs(SM0->M0_INSC,7,3)+"."+Subs(SM0->M0_INSC,10,3)                      }  //I.E
	
	oPrint:= TMSPrinter():New()
	oPrint:SetPortrait() //ou SetLandscape()
	oPrint:StartPage()   //Inicia uma nova página
	
	DbGoTop()
	ProcRegua(RecCount())
	
	DbSelectArea("SE1")
	Set Filter to
	SE1->(DbSetOrder(1))
	
	TRA->(DBGotop())
	
	While TRA->(!EOF()).And. TRA->E1_NUM <= MV_PAR04
		If !Empty(TRA->OK)
			lPrint := .T.
			nMulta := 0.01 * TRA->E1_SALDO
			xMDia  := TRA->E1_SALDO * 0.04 / 30
			         //
			//Protesto automático após 5 dias (depósito não quita boleto) - Alterado 21/08/2013
			/*
			aBolText := {"Inclusão no Serasa após 5 dias vencido (deposito não quita boleto).",;	//Encaminhar ao SERASA após 05 dias corridos do vencimento",;
				"OS: "+_SL1->L1_B_NUMOS+" CF: "+TRA->E1_NUM+". Não poderá ser quitado em nossas lojas.",;
				"Após vencimento cobrar R$ " + Alltrim(Transform(xMDia, "@E 9,999.99")) + "  por dia de atraso",;
				"Após vencimento cobrar multa de R$ " + Alltrim(Transform(nMulta, "@E 9,999.99"))}
			*/
				
			aBolText := {"MENSALIDADE SINDICAL ABRIL 2015",;
				"NAO RECEBER APOS 5 (CINCO) DIAS UTEIS",;
				"",;
				""}
			
			DbSelectArea("SA6")
			SA6->(DbSetOrder(1))// filial+ banco + agencia + conta
			DbSeek(xFilial("SA6")+MV_PAR05+MV_PAR06+MV_PAR07,.T.)
			
			xDac := ALLTRIM(SA6->A6_DVCTA)
			
			aDadosBanco  := {	SA6->A6_COD ,;   //Numero do Banco
			"ITAÚ"           				,;   //Nome do Banco
			SUBSTR(SA6->A6_AGENCIA,1,5)		,;   //Agência
			SA6->A6_NUMCON                  ,;   //Conta Corrente
			"2"                             ,;   //Carteira
			SA6->A6_COD                      }   //Codigo no SA6
			
			DbSelectArea("SA1")
			DbSeek(xFilial("SA1")+TRA->E1_CLIENTE+TRA->E1_LOJA)
			
			aDatSacado	:= { AllTrim(SA1->A1_NOME),;   //Razão Social
			AllTrim(SA1->A1_COD )   ,;   //Código
			AllTrim(SA1->A1_END )   ,;   //Endereço
			AllTrim(SA1->A1_MUN )   ,;   //Cidade
			SA1->A1_EST             ,;   //Estado
			SA1->A1_CEP				,;   //CEP
			SA1->A1_CGC             }    //CGC
			
			_nVlrAbat   :=  SomaAbat(TRA->E1_PREFIXO,TRA->E1_NUM,TRA->E1_PARCELA,"R",1,,TRA->E1_CLIENTE,TRA->E1_LOJA)
			_cPrefixo   := TRA->E1_PREFIXO
			_nTitulo    := TRA->E1_NUM
			
			IF !Empty(TRA->E1_PARCELA)
				_nTitulo  := _nTitulo + " - " + TRA->E1_PARCELA
			EndIf
			
			_dEmissao := TRA->E1_EMISSAO
			_dVenc    := TRA->E1_VENCREA
			_nSaldo   := TRA->E1_SALDO
			xParcela  := TRA->E1_PARCELA
			CB_RN_NN  := Ret_cBarra( Subs(aDadosBanco[1],1,3)+"9" , aDadosBanco[3] , ALLTRIM(aDadosBanco[4])+ALLTRIM(xDac) ,;
				aDadosBanco[5],AllTrim(TRA->E1_NUM)+AllTrim(TRA->E1_PARCELA),(TRA->E1_SALDO-_nVlrAbat),aDadosBanco[6],TRA->E1_VENCREA,TRA->E1_PREFIXO )
				
			aDadosTit := {	AllTrim(_nTitulo)    		,;  //Número do título
			_dEmissao            		,;  //Data da emissão do título
			Date()               		,;  //Data da emissão do boleto
			_dVenc               		,;  //Data do vencimento
			(_nSaldo - _nVlrAbat)		,;  //Valor do título
			CB_RN_NN[3]+"-"+CB_RN_NN[4]	,;  //Nosso Numero
			xParcela              		}   //parcela
			
			Impress(oPrint,aDadosEmp,aDadosTit,aDadosBanco,aDatSacado,aBolText,CB_RN_NN)
			n := n + 1
			i := i + 1
		EndIf
		
		TRA->(dbSkip())
		IncProc()
	EndDo
	
	oPrint:EndPage()     // Finaliza a página
	
	If lPrint
		oPrint:Preview() // Visualiza antes de imprimir
	EndIf
Return nil

/*----------------------------------------------------------------------+
|Funcao:	|	Impress              									|
+-----------------------------------------------------------------------+
|Descricao:	|	Boleto												  	|
+----------------------------------------------------------------------*/
Static Function Impress(oPrint,aDadosEmp,aDadosTit,aDadosBanco,aDatSacado,aBolText,CB_RN_NN)
	Local oFont8
	Local oFont9
	Local oFont10
	Local oFont8b
	Local oFont9b
	Local oFont10b
	Local oFont16
	Local oFont16n
	Local oFont24
	Local nRow
	Local i := 0
	Local aCoords1  := {2000,1900,2100,2300}
	Local aCoords2  := {2270,1900,2340,2300}
	Local oBrush
	Local xCarteira := "109"
	Local _cNN      := "109/" + aDadosTit[6]
	Local cMsg1     := ""
	Local cMsg2     := ""
	Local cMsg3     := ""
	
	oFont8   := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont9   := TFont():New("Arial",9,9 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10  := TFont():New("Arial",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	
	oFont8b  := TFont():New("Arial",9,8 ,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont9b  := TFont():New("Arial",9,9 ,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont10b := TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	
	oFont14  := TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont15n := TFont():New("Arial",9,15,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16  := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n := TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24  := TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	
	oPrint:StartPage()   // Inicia uma nova página
	
	//Primeira Via
	oPrint:Line (150,100,150,2300)
	oPrint:Line (150,550,50,550)
	oPrint:Line (150,800,50,800)
	
	oPrint:Say  (50,100,aDadosBanco[2],oFont15n)
	oPrint:Say  (50,567,aDadosBanco[1],oFont24)
	oPrint:Say  (70,820,CB_RN_NN[2],oFont14)
	
	oPrint:Line (220,100,220,2300 )
	oPrint:Line (290,1900,290,2300)
	oPrint:Line (360,100,360,2300 )
	oPrint:Line (425,100,425,2300 )
	oPrint:Line (490,100,490,2300 )
	
	oPrint:Line (360,500,490,500)
	oPrint:Line (425,750,490,750)
	oPrint:Line (360,1000,490,1000)
	oPrint:Line (360,1350,425,1350)
	oPrint:Line (360,1550,490,1550)
	
	cMsg1 := "ATÉ O VENCIMENTO, PREFERENCIALMENTE NO ITAÚ"
	cMsg2 := "APÓS O VENCIMENTO, SOMENTE NO ITAÚ"
	cMSg3 := ''
	
	oPrint:Say  (150,100 ,"Local de Pagamento"                          					 			,oFont8)
	oPrint:Say  (150,400 ,cMsg1                                   						       			,oFont10)
	oPrint:Say  (180,400 ,cMsg2                                         						 		,oFont10)
	oPrint:Say  (220,400 ,cMsg3                           						          	     		,oFont10)
	
	oPrint:Say  (150,1910,"Vencimento"                          	    					     		,oFont8)
	oPrint:Say  (175,2250,dtoc(aDadosTit[4])                 					          				,oFont10,,,,1)
	
	oPrint:Say  (220,1910,"Agência/Código Beneficiário" ,oFont8)
	oPrint:Say  (245,2250,ALLTRIM(aDadosBanco[3])+"/"+ALLTRIM(aDadosBanco[4])+IF(xDac <> '','-' + xDac,'') ,oFont10,,,,1)
	
	oPrint:Say  (220,100 ,"Beneficiário"                              				  		       			,oFont8)
	oPrint:Say  (245,100 ,aDadosEmp[1]                                									,oFont10)
	oPrint:Say  (295,100 ,ALLTRIM(aDadosEmp[2])+" - "+ALLTRIM(aDadosEmp[3])+" - "+ALLTRIM(aDadosEmp[4])	,oFont10)
	
	oPrint:Say  (220,1355,"CNPJ/CPF"                            				          				,oFont8)
	oPrint:Say  (245,1355,aDadosEmp[6]            				                        				,oFont10)
	
	oPrint:Say  (290,1910,"Nosso Número"                    				        	      			,oFont8)
	oPrint:Say  (315,2250,_cNN               				                    	         			,oFont10,,,,1)
	
	oPrint:Say  (360,100 ,"Data do Documento"                             	   							,oFont8)
	oPrint:Say  (385,100 ,DTOC(aDadosTit[3])                       						       			,oFont10)
	
	oPrint:Say  (360,505 ,"Número do Documento"                     					       			,oFont8)
	oPrint:Say  (385,605 ,aDadosTit[1]          					                           			,oFont10)
	
	oPrint:Say  (360,1005,"Espécie"                                        								,oFont8)
	oPrint:Say  (385,1105,"DM"                                             								,oFont10)
	
	oPrint:Say  (360,1355,"Aceite"                                  					        		,oFont8)
	oPrint:Say  (385,1455,"NÃO"                                        						     		,oFont10)
	
	oPrint:Say  (360,1555,"Data do Processamento"                        						   		,oFont8)
	oPrint:Say  (385,1655,dtoc(aDadosTit[2])                                 							,oFont10)
	
	oPrint:Say  (360,1910,"(=)Valor do Documento"                             							,oFont8)
	oPrint:Say  (385,2250,AllTrim(Transform(aDadosTit[5],"@E 999,999,999.99"))							,oFont10,,,,1)
	
	oPrint:Say  (425,100 ,"Data de Operação"  															,oFont8)
	oPrint:Say  (450,150 ,dtoc(aDadosTit[2])   															,oFont10)
	
	oPrint:Say  (425,505 ,"Carteira"                                    					    		,oFont8)
	oPrint:Say  (450,555 ,xCarteira                                    						     		,oFont10)
	
	oPrint:Say  (425,755 ,"Espécie Moeda"                               					    		,oFont8)
	oPrint:Say  (450,805 ,"R$"                                        						      		,oFont10)
	
	oPrint:Say  (425,1005,"Quantidade"                                    								,oFont8)
	oPrint:Say  (425,1555,"Valor"                    						              				,oFont8)
	
	oPrint:Say  (490,100 ,"Instruções (Todas informações deste boleto são de exclusiva responsabilidade do beneficiário.)" ,oFont8)
	oPrint:Say  (520,150 ,aBolText[1]                                						       		,oFont10)
	oPrint:Say  (570,150 ,aBolText[2]                                   					    		,oFont10)
	oPrint:Say  (620,150 ,aBolText[3]              						                         		,oFont10)
	oPrint:Say  (670,150 ,aBolText[4]                                       							,oFont10)
	
	oPrint:Say  (425,1910,"(-)Desconto"                              						       		,oFont8)
	oPrint:Say  (490,1910,"(-)Outras Deduções/Abatimento"             						      		,oFont8)
	oPrint:Say  (560,1910,"(+)Mora/Multa"                                						   		,oFont8)
	oPrint:Say  (630,1910,"(+)Outros Acréscimos"                            							,oFont8)
	oPrint:Say  (700,1910,"(=)Valor Cobrado"                  						              		,oFont8)
	
	oPrint:Say  (770,100 ,"Pagador:"                               						           		,oFont8)
	oPrint:Say  (770,400 ,aDatSacado[1]                              						       		,oFont9)
	oPrint:Say  (770,1910,"CNPJ: " + IIF(Len(AllTrim(aDatSacado[7])) == 11, Transform(AllTrim(aDatSacado[7]), "@R XXX.XXX.XXX-XX"),Transform(AllTrim(aDatSacado[7]), "@R XX.XXX.XXX/XXXX-XX")),oFont9)
	
	oPrint:Say  (818,100 ,"Endereço:"                               						           		,oFont8)
	oPrint:Say  (818,400 ,aDatSacado[3]                                     							,oFont9)
	oPrint:Say  (871,400 ,aDatSacado[4]+" - "+aDatSacado[5]+" CEP "+aDatSacado[6] 						,oFont9)
	
	//oPrint:Say  (818,1910,_cNN                                              	   						,oFont9)
	
	oPrint:Say  (922,100 ,"Sacador/Avalista:"                            		    					,oFont8)
	//oPrint:Say  (922,400 ,aDadosEmp[1]                               									,oFont9)
	oPrint:Say  (922,400 ,""			                               									,oFont9)
	
	//oPrint:Say  (922,2000 ,"Código de Baixa"                            		   						,oFont8)
	
	/*
	oPrint:Say  (962,100,"Recebimento através do cheque num.                    do banco            ." 	,oFont8)
	oPrint:Say  (962,1900,"Autenticação Mecânica"                           							,oFont8)
	oPrint:Say  (982 ,100,"Esta quitação só terá validade após o pagamento do cheque pelo" 				,oFont8)
	oPrint:Say  (1002,100,"banco pagador."																,oFont8)
	*/
	
	oPrint:Line (150,1900,770,1900 )
	oPrint:Line (560,1900,560,2300 )
	oPrint:Line (630,1900,630,2300 )
	oPrint:Line (700,1900,700,2300 )
	oPrint:Line (770,100,770,2300 )
	
	oPrint:Line (957,100,957,2300  )
	
	//MSBAR("INT25",27.2,1.2,CB_RN_NN[1],oPrint,.F.,,,,1.8,,,,.F.)    // Impressao em Jato de Tinta HP 660
	
	nRow := 300
	
	//Gambiarra, descobrir como mudar tipo da linha.
	For i := 100 to 2300 step 50
		oPrint:Line( 983+nRow, i, 983+nRow, i+30)
	Next i
	
	//Segunda Via
	oPrint:Line (1180+nRow,100,1180+nRow,2300)
	oPrint:Line (1180+nRow,550,1080+nRow,550)
	oPrint:Line (1180+nRow,800,1080+nRow,800)
	
	oPrint:Say  (1080+nRow,100,aDadosBanco[2]                    											,oFont15n)
	oPrint:Say  (1080+nRow,567,aDadosBanco[1]                 								                ,oFont24)
	oPrint:Say  (1100+nRow,820,CB_RN_NN[2]																	,oFont14)
	
	oPrint:Line (1250+nRow,100,1250+nRow,2300 )
	oPrint:Line (1320+nRow,1900,1320+nRow,2300 )
	oPrint:Line (1390+nRow,100,1390+nRow,2300 )
	oPrint:Line (1455+nRow,100,1455+nRow,2300 )
	oPrint:Line (1520+nRow,100,1520+nRow,2300 )
	
	oPrint:Line (1390+nRow,500,1520+nRow,500)
	oPrint:Line (1455+nRow,750,1520+nRow,750)
	oPrint:Line (1390+nRow,1000,1520+nRow,1000)
	oPrint:Line (1390+nRow,1350,1455+nRow,1350)
	oPrint:Line (1390+nRow,1550,1520+nRow,1550)
	
	oPrint:Say  (1180+nRow,100 ,"Local de Pagamento"                             							,oFont8)
	oPrint:Say  (1180+nRow,400 ,cMsg1                                            							,oFont10b)
	oPrint:Say  (1210+nRow,400 ,cMsg2                                            							,oFont10b)
	oPrint:Say  (1250+nRow,400 ,cMsg3                                           							,oFont10b)
	oPrint:Say  (1180+nRow,1910,"Vencimento"                                     							,oFont8)
	oPrint:Say  (1205+nRow,2250,dtoc(aDadosTit[4])                         									,oFont10b,,,,1)
	
	oPrint:Say  (1250+nRow,1910,"Agência/Código Beneficiário" 													,oFont8)
	oPrint:Say  (1275+nRow,2250,ALLTRIM(aDadosBanco[3])+"/"+ALLTRIM(aDadosBanco[4])+IIF(xDac <> '','-' + xDac,'') ,oFont10,,,,1)
	
	oPrint:Say  (1250+nRow,100 ,"Beneficiário"                                      								 ,oFont8)
	oPrint:Say  (1275+nRow,100 ,aDadosEmp[1]                               									 ,oFont10)
	oPrint:Say  (1320+nRow,100 ,ALLTRIM(aDadosEmp[2])+" - "+ALLTRIM(aDadosEmp[3])+" - "+ALLTRIM(aDadosEmp[4]) ,oFont10)
	
	oPrint:Say  (1250+nRow,1355,"CNPJ/CPF"					                    							,oFont8)
	oPrint:Say  (1275+nRow,1355,aDadosEmp[6]		                                							,oFont10)
	
	oPrint:Say  (1320+nRow,1910,"Nosso Número"                                  								,oFont8)
	oPrint:Say  (1345+nRow,2250,_cNN                                             							,oFont10,,,,1)
	
	oPrint:Say  (1390+nRow,100 ,"Data do Documento"                              							,oFont8)
	oPrint:Say  (1415+nRow,100 ,DTOC(aDadosTit[3])                               							,oFont10)
	
	oPrint:Say  (1390+nRow,505 ,"Número do Documento"                            							,oFont8)
	oPrint:Say  (1415+nRow,605 ,aDadosTit[1]                                     							,oFont10)
	
	oPrint:Say  (1390+nRow,1005,"Espécie"                                        							,oFont8)
	oPrint:Say  (1415+nRow,1105,"DM"                                             							,oFont10)
	
	oPrint:Say  (1390+nRow,1355,"Aceite"                                         							,oFont8)
	oPrint:Say  (1415+nRow,1455,"NÃO"                                            							,oFont10)
	
	oPrint:Say  (1390+nRow,1555,"Data do Processamento"                          							,oFont8)
	oPrint:Say  (1415+nRow,1655,dtoc(aDadosTit[2])                               							,oFont10)
	
	oPrint:Say  (1390+nRow,1910,"(=)Valor do Documento"                          							,oFont8)
	oPrint:Say  (1415+nRow,2250,AllTrim(Transform(aDadosTit[5],"@E 999,999,999.99")) 						,oFont10,,,,1)
	
	oPrint:Say  (1455+nRow,100 ,"Data de Operação"  															,oFont8)
	oPrint:Say  (1480+nRow,150,DTOC(aDadosTit[2] )  															,oFont10)
	
	oPrint:Say  (1455+nRow,505 ,"Carteira"                                       							,oFont8)
	oPrint:Say  (1480+nRow,555 ,xCarteira                                        							,oFont10)
	
	oPrint:Say  (1455+nRow,755 ,"Espécie Moeda"                                  							,oFont8)
	oPrint:Say  (1480+nRow,805 ,"R$"                                             							,oFont10)
	
	oPrint:Say  (1455+nRow,1005,"Quant. Moeda"                                   							,oFont8)
	oPrint:Say  (1455+nRow,1555,"Valor da Moeda"                                 							,oFont8)
	
	oPrint:Say  (1520+nRow,100 ,"Instruções (Todas informações deste boleto são de exclusiva responsabilidade do beneficiário.)" ,oFont8)
	oPrint:Say  (1550+nRow,150 ,aBolText[1]                                      							,oFont10)
	oPrint:Say  (1600+nRow,150 ,aBolText[2]                                      							,oFont10)
	oPrint:Say  (1650+nRow,150 ,aBolText[3]                                      							,oFont10)
	oPrint:Say  (1700+nRow,150 ,aBolText[4]                                      							,oFont10)
	
	oPrint:Say  (1455+nRow,1910,"(-)Desconto"                                    							,oFont8)
	oPrint:Say  (1520+nRow,1910,"(-)Outras Deduções/Abatimento"                  							,oFont8)
	oPrint:Say  (1590+nRow,1910,"(+)Mora/Multa"                                  							,oFont8)
	oPrint:Say  (1660+nRow,1910,"(+)Outros Acréscimos"                           							,oFont8)
	oPrint:Say  (1730+nRow,1910,"(=)Valor Cobrado"                               							,oFont8)
	
	oPrint:Say  (1800+nRow,100 ,"Pagador:"                                         							,oFont8)
	oPrint:Say  (1800+nRow,400 ,aDatSacado[1]                                    							,oFont9)
	oPrint:Say  (1800+nRow,1910,IIF(Len(AllTrim(aDatSacado[7])) == 11, "CPF: " + Transform(AllTrim(aDatSacado[7]), "@R XXX.XXX.XXX-XX"), "CNPJ: " + Transform(AllTrim(aDatSacado[7]), "@R XX.XXX.XXX/XXXX-XX")),oFont9)
	
	oPrint:Say  (1848+nRow,100 ,"Endereço:"                                         							,oFont8)
	oPrint:Say  (1848+nRow,400 ,aDatSacado[3]                                    							,oFont9)
	oPrint:Say  (1898+nRow,400 ,aDatSacado[4]+" - "+aDatSacado[5]+" CEP "+aDatSacado[6] 						,oFont9)
	
	//oPrint:Say  (1848+nRow,1910,_cNN                                             							,oFont9)
	
	oPrint:Say  (1952+nRow,100 ,"Sacador/Avalista:"                               							,oFont8)
	//	oPrint:Say  (1945+nRow,400 ,aDadosEmp[1]                               									,oFont9)
	oPrint:Say  (1945+nRow,400 ,""			                               									,oFont9)
	
	//oPrint:Say  (1952+nRow,2000 ,"Código de Baixa"                               							,oFont8)
	
	oPrint:Say  (1992+nRow,1900,"Ficha de Compensação"                          							,oFont9b)
	oPrint:Say  (2042+nRow,1900,"Autenticação Mecânica"                          							,oFont8)
	
	oPrint:Line (1180+nRow,1900,1800+nRow,1900 )
	oPrint:Line (1590+nRow,1900,1590+nRow,2300 )
	oPrint:Line (1660+nRow,1900,1660+nRow,2300 )
	oPrint:Line (1730+nRow,1900,1730+nRow,2300 )
	oPrint:Line (1800+nRow,100,1800+nRow,2300 )
	oPrint:Line (1987+nRow,100,1987+nRow,2300  )
	
	MSBAR("INT25",20.2,1.2,CB_RN_NN[1],oPrint,.F.,,,,1.8,,,,.F.)    // Impressao em Jato de Tinta HP 660
	
	//Gambiarra, descobrir como mudar tipo da linha.
	/*
	For i := 100 to 2300 step 50
		oPrint:Line(2213-100, i, 2213-100, i+30)
	Next i
	*/
	
	//Inicio da ultima via
	/*
	oPrint:Line (2363-100,100,2363-100,2300)
	oPrint:Line (2363-100,550,2263-100,550 )
	oPrint:Line (2363-100,800,2263-100,800 )
	
	oPrint:Say  (2263-100,100,aDadosBanco[2],oFont15n )
	oPrint:Say  (2263-100,567,aDadosBanco[1],oFont24 )
	oPrint:Say  (2263-100,820,CB_RN_NN[2],oFont14)
	
	oPrint:Line (2503-100,1900,2503-100,2300 )
	oPrint:Line (2433-100,100,2433-100,2300 )
	oPrint:Line (2573-100,100,2573-100,2300 )
	oPrint:Line (2638-100,100,2638-100,2300 )
	oPrint:Line (2703-100,100,2703-100,2300 )
	
	oPrint:Line (2573-100,500,2703-100,500)
	oPrint:Line (2638-100,750,2703-100,750)
	oPrint:Line (2573-100,1000,2703-100,1000)
	//oPrint:Line (2433-100,1350,2638-100,1350)
	oPrint:Line (2573-100,1550,2703-100,1550)
	
	oPrint:Say  (2363-100,100 ,"Local de Pagamento"                             						,oFont8)
	oPrint:Say  (2363-100,400 ,cMsg1 																	,oFont10)
	oPrint:Say  (2395-100,400 ,cMsg2 																	,oFont10)
	oPrint:Say  (2435-100,400 ,cMsg3 																	,oFont10)
	oPrint:Say  (2363-100,1910,"Vencimento"                                     						,oFont8)
	oPrint:Say  (2388-100,2250,dtoc(aDadosTit[4])                               						,oFont10,,,,1)
	
	oPrint:Say  (2433-100,1910,"Agência/Código Beneficiário"                         						,oFont8)
	oPrint:Say  (2458-100,2250,ALLTRIM(aDadosBanco[3])+"/"+ALLTRIM(aDadosBanco[4])+IIF(xDac <> '','-' + xDac,'') ,oFont10,,,,1)
	
	oPrint:Say  (2433-100,100 ,"Beneficiário"                                      							,oFont8)
	oPrint:Say  (2458-100,100 ,aDadosEmp[1]                               								,oFont10)
	oPrint:Say  (2498-100,100 ,ALLTRIM(aDadosEmp[2])+" - "+ALLTRIM(aDadosEmp[3])+" - "+ALLTRIM(aDadosEmp[4]) ,oFont10)
	
	oPrint:Say  (2433-100,1355,"CNPJ/CPF"					                  							,oFont8)
	oPrint:Say  (2458-100,1355,aDadosEmp[6]		                              							,oFont10)
	
	oPrint:Say  (2503-100,1910,"Nosso Número"                                 							,oFont8)
	oPrint:Say  (2528-100,2250,_cNN                                           							,oFont10,,,,1)
	
	oPrint:Say  (2573-100,100 ,"Data do Documento"                            							,oFont8)
	oPrint:Say  (2598-100,100 ,DTOC(aDadosTit[3])                             							,oFont10)
	
	oPrint:Say  (2573-100,505 ,"Número do Documento"                          							,oFont8)
	oPrint:Say  (2598-100,605 ,aDadosTit[1]                                   							,oFont10)
	
	oPrint:Say  (2573-100,1005,"Espécie"                                      							,oFont8)
	oPrint:Say  (2598-100,1105,"DM"                                           							,oFont10)
	
	oPrint:Say  (2573-100,1355,"Aceite"                                       							,oFont8)
	oPrint:Say  (2598-100,1455,"NAO"                                          							,oFont10)
	
	oPrint:Say  (2573-100,1555,"Data do Processamento"                        							,oFont8)
	oPrint:Say  (2598-100,1655,dtoc(aDadosTit[2])                       								,oFont10)
	
	oPrint:Say  (2573-100,1910,"(=)Valor do Documento"                        							,oFont8)
	oPrint:Say  (2598-100,2250,AllTrim(Transform(aDadosTit[5],"@E 999,999,999.99")) 					,oFont10,,,,1)
	
	oPrint:Say  (2638-100,100 ,"Uso do Banco"                             								,oFont8)
	oPrint:Say  (2663-100,150 ," "     		                          									,oFont10)
	
	oPrint:Say  (2638-100,505 ,"Carteira"                                       						,oFont8)
	oPrint:Say  (2663-100,555 ,xCarteira                                        						,oFont10)
	
	oPrint:Say  (2638-100,755 ,"Espécie Moeda"                                  						,oFont8)
	oPrint:Say  (2663-100,805 ,"R$"                                             						,oFont10)
	
	oPrint:Say  (2638-100,1005,"Quant. Moeda"                                   						,oFont8)
	oPrint:Say  (2638-100,1555,"Valor da Moeda"                                 						,oFont8)
	
	oPrint:Say  (2703-100,100 ,"Instruções (Todas informações deste boleto são de exclusiva responsabilidade do beneficiário.)" ,oFont8)
	oPrint:Say  (2733-100,150 ,aBolText[1]                                      						,oFont10)
	oPrint:Say  (2803-100,150 ,aBolText[2]                                      						,oFont10)
	oPrint:Say  (2853-100,150 ,aBolText[3]                                      						,oFont10)
	oPrint:Say  (2903-100,150 ,aBolText[4]                                      						,oFont10)
	
	oPrint:Say  (2638-100,1910,"(-)Desconto"                                    						,oFont8)
	oPrint:Say  (2703-100,1910,"(-)Outras Deduções/Abatimento"                  						,oFont8)
	oPrint:Say  (2773-100,1910,"(+)Mora/Multa"                                  						,oFont8)
	oPrint:Say  (2843-100,1910,"(+)Outros Acréscimos"                           						,oFont8)
	oPrint:Say  (2913-100,1910,"(=)Valor Cobrado"                               						,oFont8)
	
	oPrint:Say  (2983-100,100 ,"Pagador"                                         						,oFont8)
	oPrint:Say  (2983-100,400 ,aDatSacado[1]                                    						,oFont9)
	oPrint:Say  (2983-100,1910,"Cnpj- "+aDatSacado[7]                           						,oFont9)
	
	oPrint:Say  (3031-100,400 ,aDatSacado[3]                                    						,oFont9)
	oPrint:Say  (3081-100,400 ,aDatSacado[4]+" - "+aDatSacado[5]+" CEP "+aDatSacado[6] 					,oFont9)
	
	oPrint:Say  (3031-100,1910,_cNN                                             						,oFont9)
	
	oPrint:Say  (3135-100,100 ,"Sacador/Avalista"                               						,oFont8)
	//oPrint:Say  (3135-100,400 ,aDadosEmp[1]                               								,oFont9)
	
	oPrint:Say  (3135-100,2000 ,"Código de Baixa"                               						,oFont8)
	
	oPrint:Say  (3175-100,1400,"Autenticação Mecânica - Ficha de Compensação"   						,oFont8)
	
	oPrint:Line (2363-100,1900,2983-100,1900 )
	oPrint:Line (2773-100,1900,2773-100,2300 )
	oPrint:Line (2843-100,1900,2843-100,2300 )
	oPrint:Line (2913-100,1900,2913-100,2300 )
	oPrint:Line (2983-100,100,2983-100,2300 )
	
	oPrint:Line (3170-100,100,3170-100,2300  )
	*/
	
	oPrint:EndPage() // Finaliza a página
Return Nil

/*----------------------------------------------------------------------+
|Funcao:	|	Modulo10              									|
+-----------------------------------------------------------------------+
|Descricao:	|	Boleto												  	|
+----------------------------------------------------------------------*/
Static Function Modulo10(cData)
	Local L,D,P := 0
	Local B     := .F.
	L := Len(cData)
	B := .T.
	D := 0
	
	While L > 0
		P := Val(Subs(cData, L, 1))
		If (B)
			P := P * 2
			If P > 9
				P := P - 9
			EndIf
		EndIf
		D := D + P
		L := L - 1
		B := !B
	EndDo
	
	D := 10 - (Mod(D,10))
	
	IF D = 10
		D := 0
	EndIf
Return(D)

/*----------------------------------------------------------------------+
|Funcao:	|	Modulo11              									|
+-----------------------------------------------------------------------+
|Descricao:	|	Boleto												  	|
+----------------------------------------------------------------------*/
Static Function Modulo11(cData)
	Local L, D, P := 0
	L := Len(cdata)
	D := 0
	P := 1
	
	While L > 0
		P := P + 1
		D := D + (Val(Subs(cData, L, 1)) * P)
		If P = 9
			P := 1
		EndIf
		L := L - 1
	EndDo
	
	D := 11 - (mod(D,11))
	
	If (D == 0 .Or. D == 1 .Or. D == 10 .Or. D == 11)
		D := 1
	EndIf
Return(D)

/*----------------------------------------------------------------------+
|Funcao:	|	Ret_cBarra             									|
+-----------------------------------------------------------------------+
|Descricao:	|	Retorna os strings para impressão do Boleto				|
+----------------------------------------------------------------------*/
/*	CB = String para o cód.barras, RN = String com o número digitável
Cobrança não identificada, número do boleto = Título + Parcela
1     2      3       4         5       6      7         8      9*/
Static Function Ret_cBarra(cBanco,cAgencia,cConta,cCarteira,cNroDoc,nValor,cCodBanco,dVencto,cPref)
	Local bldocnufinal := STRZERO(VAL(cNroDoc),8)
	Local blvalorfinal := STRZERO((nValor*100),10)
	Local dvnn         := 0
	Local dvcb         := 0
	Local dv           := 0
	Local NN           := ""
	Local RN           := ""
	Local CB           := ""
	Local s            := ""
	Local _nFator      := dVencto - CTOD("07/10/1997")
	Local _cCart	   := "109"
	Local _cConta	   := PADL(ALLTRIM(STRTRAN(cConta,"- ","")),6,"0")

	/*-----------------------------------
	|	NOSSO NUMERO					|
	----------------------------------*/
	If Empty(TRA->E1_NUMBCO)
		cNTit := TRA->E1_PREFIXO+TRA->E1_NUM+TRA->E1_PARCELA+TRA->E1_TIPO
		
		If SE1->(MSSeek(xFilial("SE1")+cNTit))
			NN := NNItau(cNTit)
		EndIf
	Else
		NN := AllTrim(TRA->E1_NUMBCO)
	EndIf
	
	/*-----------------------------------
	|	DV do Nosso Numero				|
	----------------------------------*/
	If len(Alltrim(NN)) == 11
		NN := Strzero(Val(STRTRAN(SubStr(alltrim(NN),1,11),'X','0')),8)
	Else
		NN := SubStr(alltrim(NN),1,8)
	EndIf
	
	s:= AllTrim(cAgencia) + SubStr(alltrim(_cConta),1,5) + _cCart + SubStr(alltrim(NN),1,8)
	dvnn := modulo10(s)
	
	/*-----------------------------------
	|	CODIGO DE BARRAS				|
	----------------------------------*/
	s := cBanco + Alltrim(Str(_nFator)) + blvalorfinal + _cCart + SubStr(alltrim(NN),1,8)+Alltrim(Str(dvnn)) +AllTrim(cAgencia) + _cConta + "000"
	dvcb := modulo11(s)
	CB := SubStr(s, 1, 4) + AllTrim(Str(dvcb)) + SubStr(s, 5, 39)
	
	/*----------------------------------------------------------
	|	LINHA DIGITAVEL (Representacao Numerica)				|
	-----------------------------------------------------------*/
	/*
	CAMPO 1:
	AAA = Codigo do banco na Camara de Compensacao
	B = Codigo da moeda, sempre 9
	CCC = Codigo da Carteira de Cobranca
	DD = Dois primeiros digitos no nosso numero
	X = DAC que amarra o campo, calculado pelo Modulo 10 da String do campo
	*/
	s    := cBanco + _cCart + SUBSTR(NN,1,2)
	dv   := modulo10(s)
	RN   := SUBSTR(s, 1, 5) + '.' + SUBSTR(s, 6, 4) + AllTrim(Str(dv)) + '  '
	
	/*
	CAMPO 2:
	DDDDDD = Restante do Nosso Numero
	E = DAC do campo Agencia/Conta/Carteira/Nosso Numero
	FFF = Tres primeiros numeros que identificam a agencia
	Y = DAC que amarra o campo, calculado pelo Modulo 10 da String do campo
	*/
	s    := SUBSTR(NN,3,6) + Alltrim(Str(dvnn)) + SUBSTR(cAgencia, 1, 3)
	dv   := modulo10(s)
	RN   := RN + SUBSTR(s, 1, 5) + '.' + SUBSTR(s, 6, 5) + AllTrim(Str(dv)) + '  '
	
	/*
	CAMPO 3:
	F = Restante do numero que identifica a agencia
	GGGGGG = Numero da Conta + DAC da mesma
	HHH = Zeros (Nao utilizado)
	Z = DAC que amarra o campo, calculado pelo Modulo 10 da String do campo
	*/
	s    := SUBSTR(cAgencia, 4, 1) + _cConta + '000'
	dv   := modulo10(s)
	RN   := RN + SUBSTR(s, 1, 5) + '.' + SUBSTR(s, 6, 5) + AllTrim(Str(dv)) + '  '
	
	/*
	CAMPO 4:
	K = DAC do Codigo de Barras
	*/
	RN   := RN + AllTrim(Str(dvcb)) + '  '
	
	/*
	CAMPO 5:
	UUUU = Fator de Vencimento
	VVVVVVVVVV = Valor do Titulo
	*/
	RN   := RN + Alltrim(Str(_nFator)) + blvalorfinal
RETURN({CB,RN,NN,Alltrim(Str(dvnn))})

/*----------------------------------------------------------------------+
|Funcao:	|			            									|
+-----------------------------------------------------------------------+
|Descricao:	|	                 									  	|
+----------------------------------------------------------------------*/
Static Function ValidPerg
	_cAlias := Alias()
	
	DbSelectArea("SX1")
	dbSetOrder(1)
	
	cPerg := "ITAU      "
	aRegs:={}

	PutSX1(cPerg, "01", "Prefixo de"  , "", "", "mv_ch1" , "C", TAMSX3("E1_PREFIXO")[1] , 0, 0, "G", "",       , "", "", "mv_par01", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "02", "Prefixo ate" , "", "", "mv_ch2" , "C", TAMSX3("E1_PREFIXO")[1] , 0, 0, "G", "",       , "", "", "mv_par02", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "03", "Numero de"   , "", "", "mv_ch3" , "C", TAMSX3("E1_NUM")[1]     , 0, 0, "G", "",       , "", "", "mv_par03", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "04", "Numero ate"  , "", "", "mv_ch4" , "C", TAMSX3("E1_NUM")[1]  	, 0, 0, "G", "",       , "", "", "mv_par04", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "05", "Banco"       , "", "", "mv_ch5" , "C", 3 						, 0, 0, "G", "", "SEE" , "", "", "mv_par05", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "06", "Agencia"     , "", "", "mv_ch6" , "C", 5  						, 0, 0, "G", "",       , "", "", "mv_par06", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "07", "Conta"       , "", "", "mv_ch7" , "C", 10   					, 0, 0, "G", "",       , "", "", "mv_par07", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "08", "SubConta"    , "", "", "mv_ch8" , "C", 3   					, 0, 0, "G", "",       , "", "", "mv_par08", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "09", "Da Data?" 	  , "", "", "mv_ch9" , "D", 8						, 0, 0, "G", "",	   , "", "", "mv_par09", "","","","","","","","","","","","","","","","")
	PutSX1(cPerg, "10", "Até Data?"	  , "", "", "mv_ch10", "D", 8						, 0, 0, "G", "",       , "", "", "mv_par10", "","","","","","","","","","","","","","","","")
	
	DbSelectArea(_cAlias)
Return