package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class WarehouseOutputDTO {
    private UUID id;
    private Long orderNumber;
    private String ref;
    private String courier;
    private String warehouse;
    private Boolean status;
    private List<WarehouseOutputItemDTO> warehouseOutputItemDTOList;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String user;
}
