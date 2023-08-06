package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IDistrict;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/district")
@AllArgsConstructor
public class DistrictController {
    private final IDistrict iDistrict;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeProvince") Long codeProvince
    ) throws BadRequestExceptions {
        ResponseSuccess result = iDistrict.save(name,user, codeProvince);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/districts")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestParam("user") String user,
            @RequestParam("codeProvince") Long codeProvince,
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iDistrict.saveAll(names, user, codeProvince);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<DistrictDTO> update(
            @RequestBody() RequestDistrict requestDistrict
    ) throws BadRequestExceptions {
        DistrictDTO result = iDistrict.update(requestDistrict);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iDistrict.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/districts")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestParam("user") String user,
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iDistrict.deleteAll(codes, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<DistrictDTO>> list() throws BadRequestExceptions {
        List<DistrictDTO> result = iDistrict.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<DistrictDTO>> listStatusFalse() throws BadRequestExceptions {
        List<DistrictDTO> result = iDistrict.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<DistrictDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        DistrictDTO result = iDistrict.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<DistrictDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        DistrictDTO result = iDistrict.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/user")
    public ResponseEntity<List<DistrictDTO>> findByUser(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<DistrictDTO> result = iDistrict.findByUser(user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/province/code")
    public ResponseEntity<List<DistrictDTO>> findAllDepartmentId(
            @RequestParam("codeProvince") Long codeProvince
    ) throws BadRequestExceptions {
        List<DistrictDTO> result = iDistrict.findAllProvinceId(codeProvince);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/province/name")
    public ResponseEntity<List<DistrictDTO>> findAllDepartmentName(
            @RequestParam("nameProvince") String nameProvince
    ) throws BadRequestExceptions {
        List<DistrictDTO> result = iDistrict.findAllProvinceName(nameProvince);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
