package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.request.RequestProvinceSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.ProvinceMapper;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IProvince;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@AllArgsConstructor
@Log4j2
public class ProvinceImpl implements IProvince {

    private final ProvinceRepository provinceRepository;
    private final ProvinceMapper provinceMapper;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String name, String user, Long codeDepartment) throws BadRequestExceptions {
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            provinceRepository.save(provinceMapper.provinceToName(RequestProvinceSave.builder()
                            .codeDepártment(codeDepartment)
                            .name(name.toUpperCase())
                            .user(user.toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user, Long codeDepartment) throws BadRequestExceptions {
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            List<RequestProvinceSave> provinceSaves = names.stream().map(data -> RequestProvinceSave.builder()
                    .user(user.toUpperCase())
                    .codeDepártment(codeDepartment)
                    .name(data.toUpperCase())
                    .build()).toList();
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ProvinceDTO update(RequestProvince requestProvince) throws BadRequestExceptions {
        User datauser = userRepository.findById(requestProvince.getUser().toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            requestProvince.setName(requestProvince.getName().toUpperCase());
            requestProvince.setUser(requestProvince.getUser().toUpperCase());
            Province province = provinceRepository.save(provinceMapper.requestProvinceToProvince(requestProvince));
            province.setDateRegistration(new Date(System.currentTimeMillis()));
            return provinceMapper.provinceToProvinceDTO(province);

        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions {
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            provinceRepository.deleteByIdAndUser(code, user);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes, String user) throws BadRequestExceptions {
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            codes.stream().forEach(data -> {
                provinceRepository.deleteByIdAndUser(data, user);
            });
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<ProvinceDTO> list() throws BadRequestExceptions {
        try {
            return provinceMapper.listProvinceToListProvinceDTO(provinceRepository.findAllByStatusTrue());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<ProvinceDTO> listStatusFalse() throws BadRequestExceptions {
        try {
            return provinceMapper.listProvinceToListProvinceDTO(provinceRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public ProvinceDTO findByCode(Long code) throws BadRequestExceptions {
        try {
            return provinceMapper.provinceToProvinceDTO(provinceRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public ProvinceDTO findByName(String name) throws BadRequestExceptions {
        try {
            return provinceMapper.provinceToProvinceDTO(provinceRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<ProvinceDTO> findByUser(String user) throws BadRequestExceptions {
        try {
            return provinceMapper.listProvinceToListProvinceDTO(provinceRepository.findByUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<ProvinceDTO> findAllDepartmentId(Long codeDepartment) throws BadRequestExceptions {
        try {
            return provinceMapper.listProvinceToListProvinceDTO(provinceRepository.findAllByStatusTrueAndDepartmentId(codeDepartment));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<ProvinceDTO> findAllDepartmentName(String nameDepartment) throws BadRequestExceptions {
        try {
            return provinceMapper.listProvinceToListProvinceDTO(provinceRepository.findAllByStatusTrueAndDepartmentName(nameDepartment));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
