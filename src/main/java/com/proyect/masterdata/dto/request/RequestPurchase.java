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
    private String supplierRuc;
    private String documentName;
    private String serial;
    private String tokenUser;
    private List<RequestPurchaseItem> purchaseItemsList;
}
