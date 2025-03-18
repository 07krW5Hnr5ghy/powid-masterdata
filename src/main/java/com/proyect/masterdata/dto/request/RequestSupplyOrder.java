package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestSupplyOrder {
    private String ref;
    private String warehouse;
    private String deliveryDate;
    private String supplierRuc;
    private String purchaseDocument;
    private List<RequestSupplyOrderItem> requestSupplyOrderItemList;
}
