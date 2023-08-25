package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.request.RequestProvinceSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ProvinceMapper;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.ProvinceRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IProvince;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.List;

@Service
@AllArgsConstructor
@Log4j2
public class ProvinceImpl implements IProvince {

    private final ProvinceRepository provinceRepository;
    private final ProvinceRepositoryCustom provinceRepositoryCustom;
    private final DepartmentRepository departmentRepository;
    private final ProvinceMapper provinceMapper;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String name, String user, Long codeDepartment) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsProvince;
        boolean existsDepartment;
        try {
            existsUser = userRepository.existsById(user.toUpperCase());
            existsProvince = provinceRepository.existsByName(name.toUpperCase());
            existsDepartment = departmentRepository.existsById(codeDepartment);
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (existsProvince){
            throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
        }

        if (!existsDepartment){
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
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
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user, Long codeDepartment) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsDepartment;
        List<Province> provinces;
        try {
            existsUser = userRepository.existsById(user.toUpperCase());
            existsDepartment = departmentRepository.existsById(codeDepartment);
            provinces = provinceRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!existsDepartment){
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }
        if (!provinces.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorProvinceList.toUpperCase());
        }

        List<RequestProvinceSave> provinceSaves = names.stream().map(data -> RequestProvinceSave.builder()
                .user(user.toUpperCase())
                .codeDepártment(codeDepartment)
                .name(data.toUpperCase())
                .build()).toList();

        try {
            provinceRepository.saveAll(provinceMapper.listProvinceToListName(provinceSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ProvinceDTO update(RequestProvince requestProvince) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsDepartment;
        Province province;
        try {
            existsUser = userRepository.existsById(requestProvince.getUser().toUpperCase());
            existsDepartment = departmentRepository.existsById(requestProvince.getCodeDepartment());
            province = provinceRepository.findByNameAndStatusTrue(requestProvince.getName().toUpperCase());
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (province == null){
            throw new BadRequestExceptions(Constants.ErrorProvinceNotExist.toUpperCase());
        }

        if (!existsDepartment){
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }

        try {
            province.setName(requestProvince.getName().toUpperCase());
            province.setUser(requestProvince.getUser().toUpperCase());
            province.setDateRegistration(new Date(System.currentTimeMillis()));
            province.setStatus(requestProvince.isStatus());
            province.setIdDepartment(requestProvince.getCodeDepartment());
            return provinceMapper.provinceToProvinceDTO(provinceRepository.save(province));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        Province province;
        try {
            existsUser = userRepository.existsById(user.toUpperCase());
            province = provinceRepository.findById(code).orElse(null);
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (province == null){
            throw new BadRequestExceptions(Constants.ErrorProvinceNotExist.toUpperCase());
        }

        try {
            province.setDateRegistration(new Date(System.currentTimeMillis()));
            province.setStatus(false);
            provinceMapper.provinceToProvinceDTO(provinceRepository.save(province));
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    @Transactional
    public ResponseDelete deleteAll(List<Long> codes, String user) throws BadRequestExceptions {
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            codes.stream().forEach(data -> {
                provinceRepository.deleteByIdAndUser(data, user.toUpperCase());
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
            return provinceMapper.listProvinceToListProvinceDTO(provinceRepository.findAllByStatusTrueAndDepartmentName(nameDepartment.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
