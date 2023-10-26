package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.ModuleType;
import com.proyect.masterdata.repository.ModuleTypeRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class ModuleTypeRepositoryCustomImpl implements ModuleTypeRepositoryCustom {
    @PersistenceContext(name="entityManager")
    private EntityManager entityManager;
    @Override
    public Page<ModuleType> searchForModuleType(Long idUserType, Long idModule, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<ModuleType> criteriaQuery = criteriaBuilder.createQuery(ModuleType.class);
        Root<ModuleType> itemRoot = criteriaQuery.from(ModuleType.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(idUserType,idModule,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> moduleTypeList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                moduleTypeList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                moduleTypeList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(moduleTypeList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<ModuleType> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        long count = getOrderCount(idUserType,idModule);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }

    public List<Predicate> predicateConditions(
            Long idUserType,
            Long idModule,
            CriteriaBuilder criteriaBuilder,
            Root<ModuleType> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(idUserType!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("idUserTypeModule"),idUserType)));
        }

        if(idModule!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("idModule"),idModule)));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<ModuleType> itemRoot
    ){
        List<Order> moduleTypeList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("idUserTypeModule")){
            moduleTypeList.add(criteriaBuilder.asc(itemRoot.get("idUserTypeModule")));
        }
        if(sortColumn.equalsIgnoreCase("idModule")){
            moduleTypeList.add(criteriaBuilder.asc(itemRoot.get("idModule")));
        }
        return moduleTypeList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<ModuleType> itemRoot
    ){
        List<Order> moduleTypeList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("idUserTypeModule")){
            moduleTypeList.add(criteriaBuilder.desc(itemRoot.get("idUserTypeModule")));
        }
        if(sortColumn.equalsIgnoreCase("idModule")){
            moduleTypeList.add(criteriaBuilder.desc(itemRoot.get("idModule")));
        }
        return moduleTypeList;
    }

    private long getOrderCount(Long idUserType,Long idModule){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<ModuleType> itemRoot = criteriaQuery.from(ModuleType.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(idUserType,idModule,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
