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
public class RequestPurchase {
    private String serial;
    private String warehouse;
    private String purchaseType;
    private String purchaseDocument;
    private String supplier;
    private String purchasePaymentType;
    private List<RequestPurchaseItem> requestPurchaseItemList;
}
