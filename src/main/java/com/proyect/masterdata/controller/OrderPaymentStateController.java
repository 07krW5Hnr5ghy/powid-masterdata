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
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-payment-state")
@AllArgsConstructor
public class OrderPaymentStateController {

    private final IOrderPaymentState iOrderPaymentState;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iOrderPaymentState.save(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-states", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iOrderPaymentState.saveAll(names, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<OrderPaymentStateDTO> update(
            @RequestBody() RequestOrderPaymentState requestOrderPaymentState) throws BadRequestExceptions {
        OrderPaymentStateDTO result = iOrderPaymentState.update(requestOrderPaymentState);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iOrderPaymentState.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list")
    public ResponseEntity<List<OrderPaymentStateDTO>> listPaymentState() throws BadRequestExceptions {
        List<OrderPaymentStateDTO> result = iOrderPaymentState.listPaymentState();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
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

    @GetMapping(value = "/statusFalse")
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

    @GetMapping(value = "/code")
    public ResponseEntity<OrderPaymentStateDTO> findByCode(
            @RequestParam("code") Long code) throws BadRequestExceptions {
        OrderPaymentStateDTO result = iOrderPaymentState.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
