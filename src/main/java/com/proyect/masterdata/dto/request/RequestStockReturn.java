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
public class RequestStockReturn {
    private String purchaseSerial;
    private String warehouse;
    private List<RequestStockReturnItem> requestStockReturnItemList;
    private String tokenUser;
}
