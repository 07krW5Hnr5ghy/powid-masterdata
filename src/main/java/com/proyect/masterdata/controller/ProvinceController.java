package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IProvince;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/province")
@AllArgsConstructor
public class ProvinceController {
    private IProvince iProvince;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeDepartment") Long codeDepartment
    ) throws BadRequestExceptions {
        ResponseSuccess result = iProvince.save(name,user, codeDepartment);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/provinces")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestParam("user") String user,
            @RequestParam("codeDepartment") Long codeDepartment,
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iProvince.saveAll(names, user, codeDepartment);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<ProvinceDTO> update(
            @RequestBody() RequestProvince requestProvince
    ) throws BadRequestExceptions {
        ProvinceDTO result = iProvince.update(requestProvince);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iProvince.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/provinces")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestParam("user") String user,
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iProvince.deleteAll(codes, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<ProvinceDTO>> list() throws BadRequestExceptions {
        List<ProvinceDTO> result = iProvince.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<ProvinceDTO>> listStatusFalse() throws BadRequestExceptions {
        List<ProvinceDTO> result = iProvince.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<ProvinceDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ProvinceDTO result = iProvince.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<ProvinceDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        ProvinceDTO result = iProvince.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/user")
    public ResponseEntity<List<ProvinceDTO>> findByUser(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<ProvinceDTO> result = iProvince.findByUser(user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/department/code")
    public ResponseEntity<List<ProvinceDTO>> findAllDepartmentId(
            @RequestParam("codeDepartment") Long codeDepartment
    ) throws BadRequestExceptions {
        List<ProvinceDTO> result = iProvince.findAllDepartmentId(codeDepartment);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/department/name")
    public ResponseEntity<List<ProvinceDTO>> findAllDepartmentName(
            @RequestParam("nameDepartment") String nameDepartment
    ) throws BadRequestExceptions {
        List<ProvinceDTO> result = iProvince.findAllDepartmentName(nameDepartment);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
