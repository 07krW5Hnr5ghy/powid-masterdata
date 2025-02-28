package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.UUID;
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrderStockUpdate {
    private UUID orderStockId;
    private List<RequestOrderStockItem> requestOrderStockItemList;
    private String user;
}
