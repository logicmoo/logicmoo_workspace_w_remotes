(DEFINE (PROBLEM ASSEM-8)
   (:DOMAIN ASSEMBLY)
   (:OBJECTS GIMCRACK COIL CONTRAPTION FROB DOODAD WIRE BRACKET
             DEVICE WIDGET MOUNT KLUDGE FASTENER HACK SOCKET PLUG TUBE
             - ASSEMBLY
             HAMMOCK PLIERS - RESOURCE)
   (:INIT (AVAILABLE CONTRAPTION)
          (AVAILABLE FROB)
          (AVAILABLE DOODAD)
          (AVAILABLE WIRE)
          (AVAILABLE BRACKET)
          (AVAILABLE WIDGET)
          (AVAILABLE MOUNT)
          (AVAILABLE KLUDGE)
          (AVAILABLE HACK)
          (AVAILABLE SOCKET)
          (AVAILABLE PLUG)
          (AVAILABLE TUBE)
          (REQUIRES COIL HAMMOCK)
          (REQUIRES DEVICE PLIERS)
          (REQUIRES FASTENER HAMMOCK)
          (PART-OF COIL GIMCRACK)
          (PART-OF DEVICE GIMCRACK)
          (PART-OF FASTENER GIMCRACK)
          (TRANSIENT-PART CONTRAPTION COIL)
          (PART-OF FROB COIL)
          (PART-OF DOODAD COIL)
          (PART-OF WIRE COIL)
          (TRANSIENT-PART BRACKET COIL)
          (PART-OF WIDGET DEVICE)
          (PART-OF CONTRAPTION DEVICE)
          (PART-OF MOUNT DEVICE)
          (PART-OF KLUDGE DEVICE)
          (PART-OF BRACKET FASTENER)
          (PART-OF HACK FASTENER)
          (PART-OF SOCKET FASTENER)
          (PART-OF PLUG FASTENER)
          (PART-OF TUBE FASTENER)
          (ASSEMBLE-ORDER CONTRAPTION DOODAD COIL)
          (ASSEMBLE-ORDER CONTRAPTION FROB COIL)
          (REMOVE-ORDER DOODAD CONTRAPTION COIL)
          (ASSEMBLE-ORDER DOODAD BRACKET COIL)
          (ASSEMBLE-ORDER BRACKET FROB COIL)
          (REMOVE-ORDER FROB BRACKET COIL)
          (ASSEMBLE-ORDER WIDGET KLUDGE DEVICE)
          (ASSEMBLE-ORDER CONTRAPTION DOODAD DEVICE)
          (ASSEMBLE-ORDER CONTRAPTION FROB DEVICE)
          (ASSEMBLE-ORDER MOUNT WIDGET DEVICE)
          (ASSEMBLE-ORDER BRACKET FROB FASTENER)
          (ASSEMBLE-ORDER PLUG SOCKET FASTENER))
   (:GOAL (COMPLETE GIMCRACK)))