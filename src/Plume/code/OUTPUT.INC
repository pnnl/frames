cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     OUTPUT.INC
c
c     Contains:  The output variables
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      REAL  EOQ_hr(MaxRecp,MaxNucs), EOQ_ch(MaxRecp,MaxNucs),
     &      Dry_hr(MaxRecp,MaxNucs), Dry_ch(MaxRecp,MaxNucs),
     &      Wet_hr(MaxRecp,MaxNucs), Wet_ch(MaxRecp,MaxNucs),
     &      Shine_hr(MaxRecp), Shine_ch(MaxRecp), tot_wght
     
      COMMON /output/
     &      EOQ_hr, EOQ_ch,
     &      Dry_hr, Dry_ch,
     &      Wet_hr, Wet_ch,
     &      Shine_hr, Shine_ch, tot_wght 