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
public class RequestPurchase {
    private String ref;
    private String warehouse;
    private String purchaseType;
    private String purchaseDocument;
    private String supplier;
    private String purchasePaymentType;
    private OffsetDateTime deliveryDate;
    private List<RequestPurchaseItem> requestPurchaseItemList;
}
