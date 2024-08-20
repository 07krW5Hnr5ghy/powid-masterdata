package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestPurchaseExcel {
    private String serial;
    private String purchaseDocument;
    private String warehouse;
    private String supplier;
    private String purchaseType;
    private String tokenUser;
}
