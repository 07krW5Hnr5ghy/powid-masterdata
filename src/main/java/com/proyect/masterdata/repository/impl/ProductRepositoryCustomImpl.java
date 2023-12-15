package com.proyect.masterdata.repository.impl;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.repository.ProductRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;

@Repository
public class ProductRepositoryCustomImpl implements ProductRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Product> searchForProduct(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Product> criteriaQuery = criteriaBuilder.createQuery(Product.class);
        Root<Product> itemRoot = criteriaQuery.from(Product.class);
        criteriaQuery.select(itemRoot);
        List<Product> conditions;
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'searchForProduct'");
    }

}
