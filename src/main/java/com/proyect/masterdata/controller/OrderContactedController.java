package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderContactedDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderContacted;
import com.proyect.masterdata.services.IUtil;
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
@CrossOrigin({ "*" })
@RequestMapping("order-contacted")
@AllArgsConstructor
public class OrderContactedController {
    private final IOrderContacted iOrderContacted;
    private final IUtil iUtil;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("username") String username,
            @RequestParam("observations") String observations
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderContacted.save(orderId,username,observations);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping("mark-contacted")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("username") String username,
            @RequestParam("observations") String observations
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderContacted.markContacted(orderId,username,observations);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping("agent")
    public ResponseEntity<ResponseSuccess> agent(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("username") String username,
            @RequestParam("agentUsername") String agentUsername,
            @RequestParam("observations") String observations
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderContacted.selectAgent(orderId,username,agentUsername,observations);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping("courier")
    public ResponseEntity<ResponseSuccess> courier(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("username") String username,
            @RequestParam("courierName") String courierName,
            @RequestParam("observations") String observations
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderContacted.selectCourier(orderId,username,courierName,observations);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping(value = "list")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:COLOR_GET')")
    public ResponseEntity<Page<OrderContactedDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "orderNumber", required = false) Long orderNumber,
            @RequestParam(value = "deliveryZone",required = false) String deliveryZone,
            @RequestParam(value = "contacted", required = false) Boolean contacted,
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam(value = "updateStartDate",required = false) String uStartDate,
            @RequestParam(value = "updateEndDate",required = false) String uEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        OffsetDateTime updateStartDate = iUtil.parseToOffsetDateTime(uStartDate,true);
        OffsetDateTime updateEndDate = iUtil.parseToOffsetDateTime(uEndDate,false);
        CompletableFuture<Page<OrderContactedDTO>> result = iOrderContacted.list(
                user,
                orderNumber,
                deliveryZone,
                contacted,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
