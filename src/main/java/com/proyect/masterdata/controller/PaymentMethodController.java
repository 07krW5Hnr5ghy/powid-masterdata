package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderPaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderPaymentMethod;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/payment-method")
@AllArgsConstructor
public class PaymentMethodController {

    private final IOrderPaymentMethod iOrderPaymentMethod;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iOrderPaymentMethod.save(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-methods", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iOrderPaymentMethod.saveAll(names, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<OrderPaymentMethodDTO> update(
            @RequestBody() RequestOrderPaymentMethod requestOrderPaymentMethod) throws BadRequestExceptions {
        OrderPaymentMethodDTO result = iOrderPaymentMethod.update(requestOrderPaymentMethod);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iOrderPaymentMethod.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list-payment-method")
    public ResponseEntity<List<OrderPaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions {
        List<OrderPaymentMethodDTO> result = iOrderPaymentMethod.listPaymentMethod();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<OrderPaymentMethodDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<OrderPaymentMethodDTO> result = iOrderPaymentMethod.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/status-false")
    public ResponseEntity<Page<OrderPaymentMethodDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<OrderPaymentMethodDTO> result = iOrderPaymentMethod.listStatusFalse(name, user, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<OrderPaymentMethodDTO> findByCode(
            @RequestParam("code") Long code) throws BadRequestExceptions {
        OrderPaymentMethodDTO result = iOrderPaymentMethod.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
