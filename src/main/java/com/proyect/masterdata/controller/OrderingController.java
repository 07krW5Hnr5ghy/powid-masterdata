package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestOrderUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrdering;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FileUtils;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order")
@AllArgsConstructor
@Log4j2
public class OrderingController {

    private final IOrdering iOrdering;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:SALES') and hasAuthority('ACCESS:ORDER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestPart("requestOrder") RequestOrderSave requestOrderSave,
            @RequestPart("receipts") MultipartFile[] receipts,
            @RequestParam("tokenUser") String tokenUser
    ) throws InternalErrorExceptions, BadRequestExceptions, ExecutionException, InterruptedException, IOException {
        System.out.println(requestOrderSave);
        System.out.println(receipts);
        CompletableFuture<ResponseSuccess> result = iOrdering.saveAsync(requestOrderSave,receipts,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK') and hasAuthority('ACCESS:ORDER_GET')")
    public ResponseEntity<Page<OrderDTO>> list(
            @RequestParam(value = "orderId", required = false) UUID orderId,
            @RequestParam(value = "user") String user,
            @RequestParam(value = "seller",required = false) String seller,
            @RequestParam(value = "customer",required = false) String customer,
            @RequestParam(value = "customerPhone",required = false) String customerPhone,
            @RequestParam(value = "instagram",required = false) String instagram,
            @RequestParam(value = "departments", required = false) List<String> departments,
            @RequestParam(value = "provinces", required = false) List<String> provinces,
            @RequestParam(value = "districts", required = false) List<String> districts,
            @RequestParam(value = "saleChannels", required = false) List<String> saleChannels,
            @RequestParam(value = "receiptFlag",required = false) Boolean receiptFlag,
            @RequestParam(value = "deliveryFlag",required = false) Boolean deliveryFlag,
            @RequestParam(value = "deliveryPoints",required = false) List<String> deliveryPoints,
            @RequestParam(value = "orderStates",required = false) List<String> orderStates,
            @RequestParam(value = "couriers",required = false) List<String> couriers,
            @RequestParam(value = "paymentState",required = false) String paymentState,
            @RequestParam(value = "paymentMethod",required = false) String paymentMethod,
            @RequestParam(value = "managementType",required = false) String managementType,
            @RequestParam(value = "storeName",required = false) String storeName,
            @RequestParam(value = "sort",defaultValue = "DESC",required = false) String sort,
            @RequestParam(value = "sortColumn",defaultValue = "registrationDate",required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
            CompletableFuture<Page<OrderDTO>> result = iOrdering.list(
                orderId,
                user,
                seller,
                customer,
                customerPhone,
                instagram,
                departments,
                provinces,
                districts,
                saleChannels,
                receiptFlag,
                deliveryFlag,
                deliveryPoints,
                orderStates,
                couriers,
                paymentState,
                paymentMethod,
                managementType,
                storeName,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PutMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_PUT')")
    public ResponseEntity<ResponseSuccess> update(
            @RequestParam("orderId") UUID orderId,
            @RequestPart("requestOrder") RequestOrderUpdate requestOrderUpdate,
            @RequestPart("receipts") MultipartFile[] receipts,
            @RequestPart("courierPictures") MultipartFile[] courierPictures,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrdering.updateAsync(orderId,requestOrderUpdate,receipts,courierPictures,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK') and hasAuthority('ACCESS:ORDER_GET')")
    public ResponseEntity<List<OrderDTO>> listOrders(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderDTO>> result = iOrdering.listOrder(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("detail")
    public ResponseEntity<OrderDTO> detail(
            @RequestParam("user") String user,
            @RequestParam("orderId") UUID orderId
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<OrderDTO> result = iOrdering.selectOrder(orderId,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
