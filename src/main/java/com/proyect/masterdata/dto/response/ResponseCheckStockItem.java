package com.proyect.masterdata.dto.response;

import com.proyect.masterdata.dto.CheckStockItemDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ResponseCheckStockItem {
    private Boolean pendingStock;
    private String message;
    private Integer pendingQuantity;
    private List<CheckStockItemDTO> itemStockList;
}
