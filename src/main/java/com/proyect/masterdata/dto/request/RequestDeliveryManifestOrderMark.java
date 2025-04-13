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
public class RequestDeliveryManifestOrderMark {
    private String username;
    private List<UUID> orderIds;
    private UUID deliveryManifestId;
}
