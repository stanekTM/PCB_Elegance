/*
 * PCB elegance (Open source tools for making printed circuit boards)
 *
 * Copyright (C) 2012  Herman Morsink Vollenbroek
 *
 * File: change.c
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
/*******************************************************************************************/


#include "types.h"
#include "memory.h"
#include "graphics.h"
#include "string.h"
#include "toets.h"
#include "mainloop.h"
#include "edit.h"
#include "edit2.h"
#include "line2.h"
#include "draw.h"
#include "draw2.h"
#include "draw3.h"
#include "ellipss.h"
#include "sch.h"
#include "calc.h"
#include "math.h"
#include "calc2.h"
#include "calcdef.h"
#include "calcrect.h"
#include "insdel.h"
#include "files.h"
#include "dialogs.h"
#include "insdel.h"
#include "select.h"
#include "resource.h"
#include "property.h"


// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ChangeText(int32 Mode)
{
	int32 cnt, TempLastActionNr, PinMode, Changed1, Changed2, TextChanged, Changed;
	NetLabelRecord *NetLabel;
	GlobalConnectionRecord *GlobalConnection, ChangedGlobalConnection;
	ObjectTextRecord *ObjectText, ChangedObjectText;
	InstanceRecord *Instance, ChangedInstance;
	PinRecord *Pin, ChangedPin;
	PowerPinRecord *PowerPin, ChangedPowerPin;
	PinBusRecord *PinBus, ChangedPinBus;
	ObjectTextRecord ChangedText1, ChangedText2;


	GetNrSelections();

	if ((EditingSymbol) && (InstanceRefsSelected == 0) && (ObjectTextsSelected == 0) && (PinsSelected == 0)
	        && (PowerPinsSelected == 0) && (PinBussesSelected == 0))
	{
		PostMessage(SCHWindow, WM_COMMAND, (WPARAM) ID_EDIT_SYMBOLNAMES, (LPARAM) NULL);
		return;
	}

	Changed = 0;
	TextChanged = 0;
	TempLastActionNr = LastActionNr - 1;

	if (InstancesSelected == 0)
	{
		if ((InstanceRefsSelected > 0) || (InstanceValuesSelected > 0))
		{
			for (cnt = 0; cnt < Design.NrInstances; cnt++)
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);

				if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (Instance->AddNr <= TempLastActionNr))
				{
					if ((Instance->Info & 4) == 0)
					{
						Changed1 = 0;
						Changed2 = 0;

						if ((Instance->Info & 1) == 1)
						{
							memset(&ChangedText1, 0, sizeof(ChangedText1));
							memmove(&ChangedText1.Text, Instance->Reference, sizeof(Instance->Reference) - 1);

							if ((Instance->RefInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
								ChangedText1.Info |= TEXT_NOT_VISIBLE;

							if (TextInputDialog(&ChangedText1, 2) == 1)
								Changed1 = 1;
						}

						if ((Instance->Info & 2) == 2)
						{
							memset(&ChangedText2, 0, sizeof(ChangedText2));
							memmove(&ChangedText2.Text, Instance->Value, sizeof(Instance->Value) - 1);

							if ((Instance->ValueInfo & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
								ChangedText2.Info |= TEXT_NOT_VISIBLE;

							if (TextInputDialog(&ChangedText2, 3) == 1)
								Changed2 = 1;
						}

						if ((Changed1) || (Changed2))
						{
							memmove(&ChangedInstance, Instance, sizeof(InstanceRecord));

							if (Changed1)
							{
								memset(&ChangedInstance.Reference, 0, sizeof(Instance->Reference));
								memmove(&ChangedInstance.Reference, &ChangedText1.Text,
								        sizeof(ChangedInstance.Reference) - 1);

								if ((ChangedText1.Info & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
									ChangedInstance.RefInfo |= TEXT_NOT_VISIBLE;
								else
									ChangedInstance.RefInfo &= ~TEXT_NOT_VISIBLE;
							}

							if (Changed2)
							{
								memset(&ChangedInstance.Value, 0, sizeof(ChangedInstance.Value));
								memmove(&ChangedInstance.Value, &ChangedText2.Text, sizeof(ChangedInstance.Value) - 1);

								if ((ChangedText2.Info & TEXT_NOT_VISIBLE) == TEXT_NOT_VISIBLE)
									ChangedInstance.ValueInfo |= TEXT_NOT_VISIBLE;
								else
									ChangedInstance.ValueInfo &= ~TEXT_NOT_VISIBLE;
							}

							ChangedInstance.Info &= ~(OBJECT_SELECTED | 7);

							if (AddInstance(&ChangedInstance))
							{
								Instance = (InstanceRecord *) & ((*Instances)[cnt]);
								Instance->Info |= OBJECT_NOT_VISIBLE;
								Instance->DeleteNr = (int16) LastActionNr;
								TextChanged = 1;
								Changed = 1;
							}
						}
					}
				}
			}
		}
	}
	else
	{
		if (!EditingSymbol)
			EditInstanceInfo(0);
	}

	if (Mode == 0)
	{
		if (NetLabelsSelected > 0)
		{
			for (cnt = 0; cnt < Design.NrNetLabels; cnt++)
			{
				NetLabel = &((*NetLabels)[cnt]);

				if (((NetLabel->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (NetLabel->AddNr <= TempLastActionNr))
				{
					if (ModifyNetProperty(cnt, 1))
					{
						TextChanged = 1;
						Changed = 1;
					}

					/*
					          memmove(&ChangedNetLabel,NetLabel,sizeof(NetLabelRecord));
					          if (AddNetlabelDialog(&ChangedNetLabel,1)==1) {
					            ChangedNetLabel.Info&=~OBJECT_SELECTED;
					            if (AddNetLabel(&ChangedNetLabel)) {
					              NetLabel=&((*NetLabels)[cnt]);
					              NetLabel->Info|=OBJECT_NOT_VISIBLE;
					              NetLabel->DeleteNr=(int16)LastActionNr;
					              TextChanged=1;
					              Changed=1;
					            }
					          }
					*/
				}
			}
		}

		if (GlobalConnectionsSelected + GlobalConnectionTextsSelected > 0)
		{
			for (cnt = 0; cnt < Design.NrGlobalConnections; cnt++)
			{
				GlobalConnection = &((*GlobalConnections)[cnt]);

				if (((GlobalConnection->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (GlobalConnection->AddNr <= TempLastActionNr))
				{
					memmove(&ChangedGlobalConnection, GlobalConnection, sizeof(GlobalConnectionRecord));
					memset(&ChangedText1.Text, 0, sizeof(ChangedText1.Text));
					memmove(&ChangedText1.Text, GlobalConnection->Text, sizeof(GlobalConnection->Text) - 1);

					if (TextInputDialog(&ChangedText1, 5) == 1)
					{
						memset(&ChangedGlobalConnection.Text, 0, sizeof(GlobalConnection->Text));
						memmove(&ChangedGlobalConnection.Text, &ChangedText1.Text,
						        sizeof(ChangedGlobalConnection.Text) - 1);
						ChangedGlobalConnection.Info &= ~OBJECT_SELECTED;

						if (AddGlobalConnection(&ChangedGlobalConnection))
						{
							GlobalConnection = &((*GlobalConnections)[cnt]);
							GlobalConnection->Info |= OBJECT_NOT_VISIBLE;
							GlobalConnection->DeleteNr = (int16) LastActionNr;
							TextChanged = 1;
							Changed = 1;
						}
					}
				}
			}
		}

		if (ObjectTextsSelected > 0)
		{
			for (cnt = 0; cnt < Design.NrObjectTexts; cnt++)
			{
				ObjectText = &((*ObjectTexts)[cnt]);

				if (((ObjectText->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (ObjectText->AddNr <= TempLastActionNr))
				{
					memmove(&ChangedObjectText, ObjectText, sizeof(ObjectTextRecord));

					if (TextInputDialog(&ChangedObjectText, 13) == 1)
					{
						ChangedObjectText.Info &= ~OBJECT_SELECTED;

						if (ChangedObjectText.FontHeight < 0.0)
							ChangedObjectText.FontHeight = ObjectText->FontHeight;

						if (AddObjectText(&ChangedObjectText))
						{
							ObjectText = &((*ObjectTexts)[cnt]);
							ObjectText->Info |= OBJECT_NOT_VISIBLE;
							ObjectText->DeleteNr = (int16) LastActionNr;
							TextChanged = 1;
							Changed = 1;
						}
					}
				}
			}
		}

		if (PinsSelected > 0)
		{
			for (cnt = 0; cnt < DesignSymbol.NrPins; cnt++)
			{
				Pin = &((*Pins)[cnt]);

				if (((Pin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (Pin->AddNr <= TempLastActionNr))
				{
					memmove(&ChangedPin, Pin, sizeof(PinRecord));

					if (!EditingSheetSymbol)
						PinMode = 1;
					else
						PinMode = 3;

					if (AddPinsDialog(&ChangedPin, PinMode) == 1)
					{
						ChangedPin.Info &= ~(OBJECT_SELECTED | 3);

						if (AddPin(&ChangedPin))
						{
							Pin = &((*Pins)[cnt]);
							Pin->Info |= OBJECT_NOT_VISIBLE;
							Pin->DeleteNr = (int16) LastActionNr;
							TextChanged = 1;
							Changed = 1;
						}
					}
				}
			}
		}

		if (PowerPinsSelected > 0)
		{
			for (cnt = 0; cnt < DesignSymbol.NrPowerPins; cnt++)
			{
				PowerPin = &((*PowerPins)[cnt]);

				if (((PowerPin->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (PowerPin->AddNr <= TempLastActionNr))
				{
					memmove(&ChangedPowerPin, PowerPin, sizeof(PowerPinRecord));

					if (AddPowerPinsDialog(&ChangedPowerPin, 1) == 1)
					{
						ChangedPowerPin.Info &= ~OBJECT_SELECTED;

						if (AddPowerPin(&ChangedPowerPin))
						{
							PowerPin = &((*PowerPins)[cnt]);
							PowerPin->Info |= OBJECT_NOT_VISIBLE;
							PowerPin->DeleteNr = (int16) LastActionNr;
							TextChanged = 1;
							Changed = 1;
						}
					}
				}
			}
		}

		if (PinBussesSelected > 0)
		{
			for (cnt = 0; cnt < DesignSymbol.NrPinBusses; cnt++)
			{
				PinBus = &((*PinBusses)[cnt]);

				if (((PinBus->Info & (OBJECT_NOT_VISIBLE | OBJECT_SELECTED)) == OBJECT_SELECTED)
				        && (PinBus->AddNr <= TempLastActionNr))
				{
					memmove(&ChangedPinBus, PinBus, sizeof(PinBusRecord));

					if (AddPinBusDialog(&ChangedPinBus, 1) == 1)
					{
						ChangedPinBus.Info &= ~(OBJECT_SELECTED | 3);

						if (AddPinBus(&ChangedPinBus))
						{
							PinBus = &((*PinBusses)[cnt]);
							PinBus->Info |= OBJECT_NOT_VISIBLE;
							PinBus->DeleteNr = (int16) LastActionNr;
							TextChanged = 1;
							Changed = 1;
						}
					}
				}
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void UnprotectSymbols()
{
	int32 cnt, TempLastActionNr;
	InstanceRecord *Instance, ChangedInstance;
	int32 Changed;

	Changed = 0;
	TempLastActionNr = LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED)) == OBJECT_PROTECTED)
		        && (Instance->AddNr <= TempLastActionNr))
		{
			memmove(&ChangedInstance, Instance, sizeof(InstanceRecord));
			ChangedInstance.Info &= ~(OBJECT_PROTECTED);

			if (AddInstance(&ChangedInstance))
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);
				Instance->Info |= OBJECT_NOT_VISIBLE;
				Instance->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************

void ProtectSymbols()
{
	int32 cnt, TempLastActionNr;
	InstanceRecord *Instance, ChangedInstance;
	int32 Changed;

	Changed = 0;

	TempLastActionNr = LastActionNr - 1;

	for (cnt = 0; cnt < Design.NrInstances; cnt++)
	{
		Instance = (InstanceRecord *) & ((*Instances)[cnt]);

		if (((Instance->Info & (OBJECT_NOT_VISIBLE | OBJECT_PROTECTED | OBJECT_SELECTED)) == OBJECT_SELECTED)
		        && (Instance->AddNr <= TempLastActionNr))
		{
			memmove(&ChangedInstance, Instance, sizeof(InstanceRecord));
			ChangedInstance.Info |= OBJECT_PROTECTED;
			ChangedInstance.Info &= ~OBJECT_SELECTED;

			if (AddInstance(&ChangedInstance))
			{
				Instance = (InstanceRecord *) & ((*Instances)[cnt]);
				Instance->Info |= OBJECT_NOT_VISIBLE;
				Instance->DeleteNr = (int16) LastActionNr;
				Changed = 1;
			}
		}
	}

	if (Changed)
		RePaint();
}

// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
// ********************************************************************************************************
