package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderPaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderPaymentState;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-payment-state")
@AllArgsConstructor
public class OrderPaymentStateController {

    private final IOrderPaymentState iOrderPaymentState;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_STATE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iOrderPaymentState.save(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "order-payment-states", consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_STATE_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> names,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iOrderPaymentState.saveAll(names, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_STATE_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iOrderPaymentState.delete(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_STATE_GET')")
    public ResponseEntity<List<OrderPaymentStateDTO>> listPaymentState() throws BadRequestExceptions {
        List<OrderPaymentStateDTO> result = iOrderPaymentState.listPaymentState();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_STATE_GET')")
    public ResponseEntity<Page<OrderPaymentStateDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<OrderPaymentStateDTO> result = iOrderPaymentState.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_STATE_GET')")
    public ResponseEntity<Page<OrderPaymentStateDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<OrderPaymentStateDTO> result = iOrderPaymentState.listStatusFalse(name, user, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
