package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IDeliveryManifestItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("delivery-manifest-item")
@AllArgsConstructor
public class DeliveryManifestItemController {
    private final IDeliveryManifestItem iDeliveryManifestItem;
    @PutMapping("/{deliveryManifestItemId}")
    public ResponseEntity<ResponseSuccess> closeManifest(
            @PathVariable UUID deliveryManifestItemId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryManifestItem.updateDeliveryManifestItem(deliveryManifestItemId,user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<Page<DeliveryManifestItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "quantity",required = false) Integer quantity,
            @RequestParam(value = "collected",required = false) Boolean collected,
            @RequestParam(value = "orderNumber",required = false) Long orderNumber,
            @RequestParam(value = "manifestNumber",required = false) Long manifestNumber,
            @RequestParam(value = "color",required = false) String color,
            @RequestParam(value = "size",required = false) String size,
            @RequestParam(value = "model",required = false) String model,
            @RequestParam(value = "supplier",required = false) String supplier,
            @RequestParam(value = "brand",required = false) String brand,
            @RequestParam(value = "deliveryStatus",required = false) String deliveryStatus,
            @RequestParam(value = "courier",required = false) String courier,
            @RequestParam(value = "warehouse",required = false) String warehouse,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<DeliveryManifestItemDTO>> result = iDeliveryManifestItem.list(
                user,
                quantity,
                collected,
                orderNumber,
                manifestNumber,
                color,
                size,
                model,
                supplier,
                brand,
                deliveryStatus,
                courier,
                warehouse,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
