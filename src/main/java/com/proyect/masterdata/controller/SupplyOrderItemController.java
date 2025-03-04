package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SupplyOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestSupplyOrderItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISupplyOrderItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("supply-order-item")
@AllArgsConstructor
public class SupplyOrderItemController {
    private final ISupplyOrderItem iSupplyOrderItem;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("purchaseId") UUID purchaseId,
            @RequestBody() RequestSupplyOrderItem requestSupplyOrderItem,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, InterruptedException, ExecutionException {
        CompletableFuture<ResponseSuccess> result = iSupplyOrderItem.saveAsync(purchaseId, requestSupplyOrderItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<Page<SupplyOrderItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "purchaseNumber",required = false) Long purchaseNumber,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "model",required = false) String model,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<SupplyOrderItemDTO>> result = iSupplyOrderItem.list(
                user,
                purchaseNumber,
                warehouse,
                model,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<List<SupplyOrderItemDTO>> listPurchase(
            @RequestParam("user") String user,
            @RequestParam(value = "id", required = false) UUID id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplyOrderItemDTO>> result = iSupplyOrderItem.listPurchaseItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
