package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUserRole;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/user-role")
@AllArgsConstructor
public class UserRoleController {
    private IUserRole iUserRole;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iUserRole.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/user-roles")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iUserRole.saveAll(names,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<UserRoleDTO> update(
            @RequestBody() RequestUserRole requestUserRole
    ) throws BadRequestExceptions {
        UserRoleDTO result = iUserRole.update(requestUserRole);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iUserRole.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/user-roles")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iUserRole.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<UserRoleDTO>> list() throws BadRequestExceptions {
        List<UserRoleDTO> result = iUserRole.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<UserRoleDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        UserRoleDTO result = iUserRole.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<UserRoleDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        UserRoleDTO result = iUserRole.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
