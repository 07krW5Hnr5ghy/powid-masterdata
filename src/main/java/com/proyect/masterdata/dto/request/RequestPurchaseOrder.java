package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestPurchaseOrder {
    private String ref;
    private String supplierRuc;
    private List<RequestPurchaseOrderItem> requestPurchaseOrderItemList;
}
