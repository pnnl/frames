*-----------------------------------------------------------------------------*
*
      block data
*
*-----------------------------------------------------------------------
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      data organ /'Adrenals ', 'Bld Wall ', 'B Surface', 'Brain    ',
     :            'Breasts  ', 'Esophagus', 'St Wall  ', 'SI Wall  ',
     :            'ULI Wall ', 'LLI Wall ', 'Kidneys  ', 'Liver    ',
     :            'Lungs    ', 'Muscle   ', 'Ovaries  ', 'Pancreas ',
     :            'R Marrow ', 'Skin     ', 'Spleen   ', 'Testes   ',
     :            'Thymus   ', 'Thyroid  ', 'Uterus   ', '  E(Sv)  ' /
      data indorg / 1,  2,  3,  4,  5, 27,  6,  7,  8,  9, 10, 11,
     :             31, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 33 /
      data iextorg/ 2, 23, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 
     :             25, 16, 17, 1, 18, 19, 20, 21, 22, 24, 27 /
      data cancer/'Esophagus', 'Stomach  ', 'Colon    ', 'Liver    ',
     :            'Lung     ', 'Bone     ', 'Skin     ', 'Breast   ',
     :            'Ovary    ', 'Bladder  ', 'Kidneys  ', 'Thyroid  ',
     :            'Leukemia ', 'Residual ', 'Total    '/
      data namage/'Infant   ', '1 yr old ', '5 yr old ', '10 yr old',
     :            '15 yr old', 'Adult    '/
      end
