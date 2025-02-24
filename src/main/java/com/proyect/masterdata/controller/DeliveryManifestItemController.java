package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IDeliveryManifestItem;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
}
