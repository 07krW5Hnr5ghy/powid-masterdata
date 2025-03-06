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
public class RequestWarehouseOutput {
    private String warehouse;
    private String ref;
    private List<RequestWarehouseOutputItem> requestWarehouseOutputItemList;
    private String username;
}
