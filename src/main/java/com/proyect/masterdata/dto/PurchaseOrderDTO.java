package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PurchaseOrderDTO {
    private UUID id;
    private Long orderNumber;
    private String ref;
    private Boolean status;
    private List<PurchaseOrderItemDTO> purchaseOrderItemDTOList;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private OffsetDateTime deliveryDate;
    private String user;
}
