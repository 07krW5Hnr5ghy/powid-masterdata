package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DeliveredOrdersCountDTO {
    private UUID deliveredManifestId;
    private UUID orderId;
    private Long deliveredCount;
}
